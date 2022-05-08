library(tidyverse)
library(lubridate)
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(plotly)
library(shinydashboard)
library(janitor)
library(httr)
library(fontawesome)
library(crosstalk)


# load and clean data

air_dat <- read.csv("../data/2960014368-latest.csv", sep = ";")



air_dat_cleaned <- air_dat %>% 
  clean_names() %>%
  mutate(recorded = str_replace(recorded, "T", " "), 
         recorded = case_when(str_detect(recorded, "\\d*:\\d*:") ~ 
                                with_tz(ymd_hms(recorded), tz = "America/New_York"),
                              str_detect(recorded, "(?<!:)\\d{2}:\\d{2}$") ~ 
                                with_tz(ymd_hm(recorded), tz = "America/New_York"))
         ) %>% 
  separate(recorded, sep = " ", into = c("date", "time"), remove = FALSE) %>% 
  mutate(date = ymd(date),
         time = hms(time),
         Time = ifelse(minute(time) >= 10, paste0(hour(time), ":", minute(time)),
                       paste0(hour(time), ":0", minute(time)))) 


air_dat_long <- air_dat_cleaned %>% 
  pivot_longer(names_to = "metric",
               values_to = "Result",
               -c("recorded", "date", "time", "Time"),
               values_drop_na = TRUE) %>% 
  mutate(metric = case_when(metric == "co2_ppm" ~ "CO2 (ppm)",
                            metric == "pm1_mg_m3" ~ "PM10 (ug/m3)",
                            metric == "pm2_5_mg_m3" ~ "PM2.5 (ug/m3)",
                            metric == "pressure_h_pa" ~ "Pressure (mbar)",
                            metric == "radon_short_term_avg_p_ci_l" ~ "Radon (pCi/L)",
                            metric == "temp_f" ~ "Temperature (F)",
                            metric == "voc_ppb" ~ "VOC (ppb)",
                            metric == "humidity" ~ "Humidity (%)")) %>%  
  #drop first 7 days of VOC and CO2 (calibration period)
  filter(!(metric %in% c("VOC (ppb)", "CO2 (ppm)") & date < c(min(date)+days(7)))) %>% 
  group_by(metric) %>% 
  #look at the record before and after 
  mutate(rec_prior= lag(recorded, order_by = recorded, 
                        default = min(recorded)),
         rec_after = lead(recorded, order_by = recorded, 
                          default = max(recorded)),
         intrvl = interval(rec_prior, rec_after),
         dif = as.duration(intrvl)/2
  ) 





###############################################################################
###### GET TIME RANGE DATA  ##################################################
###############################################################################


#12hr 
dat_12hr <-  air_dat_long %>%
  filter(recorded >= max(air_dat_long$recorded)-hours(12)) 

dat_24hr <-   air_dat_long %>%
  filter(recorded >= max(air_dat_long$recorded) - hours(24)) 

dat_36hr <- air_dat_long %>%
  filter(recorded >= max(air_dat_long$recorded) - hours(36)) 

dat_48hr <- air_dat_long %>%
  filter(recorded >= max(air_dat_long$recorded) - hours(48)) 

dat_1wk <- air_dat_long %>%
  filter(recorded >= max(air_dat_long$recorded)- weeks(1)) 

dat_1mo <- air_dat_long %>%
  filter(recorded >= max(air_dat_long$recorded) - months(1))

###############################################################################
###### ADD FUNCTIONS FOR INDIV METRIC PAGES ###################################
###############################################################################


time_subplot <- function(ggdat, facet){
  sub_dat <- ggdat %>% 
    filter(metric == facet)
  
  sub_plot <- ggplot(sub_dat) +
    geom_line(aes(x = recorded, y = Result ,
                  color = day(recorded),
                  label = Time,
                  label2 = date)) +
    viridis::scale_color_viridis() +
    facet_wrap(~metric, ncol = 1, scales = "free_y") +
    ggthemes::theme_pander() +
    xlab("Date")+
    ylab("Concentration") +
    theme(legend.position = "none",
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(color = "snow2"),
          strip.text.x = element_text(color = "#556B2F", face = "bold"),
          text = element_text(family = "Arial"))
  
  sub_plotly <- ggplotly(sub_plot,
                         tooltip = c("y", "label", "label2"))
  
  sub_violin_ly <- chronicle::make_violin(sub_dat, value = 'Result', 
                                        plot_palette = "#ec7f78")
  
  sub_box <- ggplot(sub_dat) + 
    geom_boxplot(aes(x = "Current\nRange", y = Result)) + 
    geom_boxplot(air_dat_long %>%
                   filter(metric == facet), mapping = aes(x = "All\nMeasurements", y = Result)) +
    ggthemes::theme_pander() +
    theme(legend.position = "none",
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(color = "snow2"), 
          text = element_text(family = "Arial"))
  
  sub_box_ly <- ggplotly(sub_box)
  
  subplot(list(sub_plotly, sub_box_ly), widths = c(.9, .1), 
          nrows = 1, shareY = TRUE, margin = 0)
}

dens_plotly <- function(ggdat, facet){
  dens_dat <- ggdat %>% 
    filter(metric == facet)
  
  dens_plot <- ggplot(dens_dat, aes(x = Result)) +
    geom_histogram(aes(y = ..density..),
                   color = "black",
                   fill = "white",
                   bins = 30) +
    geom_density(fill = "red", alpha = 0.25) + 
    facet_wrap(~metric) + 
    ggthemes::theme_pander() +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(color = "snow2"),
          strip.text.x = element_text(color = "#556B2F", face = "bold"),
          text = element_text(family = "Arial"))
  
  #print figure
  ggplotly(dens_plot)
}

comp_xy <- function(ggdat, facet, input){
  
  new_dat <- air_dat_long %>% 
    select(-recorded) %>% 
    mutate(hour_match = hour(time),
           minute_match = minute(time)) %>%
    select(metric, date, hour_match, minute_match, Result) %>% 
    pivot_wider(names_from = "metric", values_from = "Result") %>% 
    select(all_of(`facet`), all_of(input), date, hour_match, minute_match) %>% 
    rename(x = facet,
           y = input)

  
  if(input != facet){
    xy_plot <- ggplot(new_dat, aes(x = x, y = y)) +
      geom_point() + 
      xlab(paste0(facet))+
      ylab(paste0(input))
  }
  else{
    xy_plot <- ggplot(new_dat, aes(x = y, y = y)) +
      geom_point() + 
      xlab(paste0(facet))+
      ylab(paste0(input))
  }

  
  ggplotly(xy_plot)
}

cooking_log <- readxl::read_xlsx("../data/cooking log.xlsx") %>% 
  filter(!is.na(fan)) %>% 
  rownames_to_column(var = "event_no") %>%
  mutate(event_no = factor(event_no, levels=unique(.$event_no)),
         meal = tolower(meal)) %>% 
  separate(time, "-", into = c("start", "end")) %>% 
  mutate(start_fmt = ymd_hm(paste(date, start), tz = "EST"), 
         end_fmt = ymd_hm(paste(date, end), tz = "EST"), 
         date = as_date(date), 
         start = hm(start), 
         end = hm(end),
         start_shift = start+hours(-3),
         end_shift = end+hours(3), 
         intrvl = interval(start_fmt, end_fmt, tzone = "EST"),
         intrvl_shft_start = int_start(int_shift(intrvl, duration(hour = c(-3)))),
         intrvl_shft_end = int_end(int_shift(intrvl, duration(hour = c(3)))),
         intrvl_shft = interval(intrvl_shft_start, intrvl_shft_end)) %>% 
  mutate(intervention = case_when(tolower(fan) == "yes" & tolower(window) == "open" ~ "fan + window",
                                  tolower(fan) == "yes" ~ "fan only",
                                  tolower(window) == "open" ~ "window only",
                                  TRUE ~ "no intervention"), 
         intervention = factor(intervention, levels = c("no intervention", 
                                                        "fan only",
                                                        "window only", 
                                                        "fan + window")), 
         intervention_sa = case_when(intervention %in% c("window only", "fan + window") ~ "window open",
                                     TRUE ~ as.character(intervention)),
         intervention_sa = factor(intervention_sa, levels = c("no intervention", 
                                                              "fan only",
                                                              "window open")))
cooking_log_shiny <- cooking_log %>% 
  mutate(start = ifelse(minute(start) >= 10, paste0(hour(start), ":", minute(start)),
                       paste0(hour(start), ":0", minute(start))),
         end = ifelse(minute(end) >= 10, paste0(hour(end), ":", minute(end)),
                       paste0(hour(end), ":0", minute(end)))) %>% 
  select(event_no, date, start, end, meal, food, intervention) %>% 
  rename(`Cooking Event ID` = event_no,
         Date = date,
         `Cooking start time` = start,
         `Cooking end time` = end,
         Meal = meal,
         Food = food,
         `Intervention category` = intervention
         )


cooking_plot_dat <- air_dat_long %>%
  filter(metric %in% c("CO2 (ppm)", "PM10 (ug/m3)", "PM2.5 (ug/m3)")) %>%
  mutate(metric = factor(metric)) %>%
  filter(as_date(date) %in% c(cooking_log$date)) %>%
  left_join(cooking_log, by = c("date")) %>%
  group_by(recorded) %>%
  mutate(keep = case_when(time >= start_shift & time <= end_shift ~ "yes",
                          TRUE ~ "no")) %>%
  filter(keep == "yes")


time_passed <- air_dat_long %>% 
  filter(metric %in% c("CO2 (ppm)", "PM10 (ug/m3)", "PM2.5 (ug/m3)")) %>% 
  left_join(cooking_log, by = c("date")) %>% 
  group_by(recorded) %>% 
  mutate(active_cooking = case_when(time >= start & time <= end_shift ~ "yes",
                                    TRUE ~ "no")) %>% 
  filter(active_cooking == "yes") %>% 
  group_by(event_no, metric, intervention) %>% 
  mutate(newtime = cumsum(dif)/60)

avg_time_passed <- time_passed %>% 
  group_by(round(newtime, 0), intervention, metric) %>%
  summarize(Result = mean(Result)) %>% 
  rename(newtime = `round(newtime, 0)`)

