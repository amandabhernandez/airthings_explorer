### AUTHOR: AHz
### LAST EDIT: 2022-04-26
### WRITTEN IN: R version 4.0.5
### Purpose: work with AirThings data 


###############################################################################
# 0. SETUP  ##################################################################
###############################################################################

library(pacman)
#p_load installs package if its not already installed, otherwise loads it

p_load(rstudioapi)
p_load(tidyverse)
p_load(lubridate)
p_load(janitor)
p_load(ggthemes)
p_load(viridis)
p_load(ggforce)

# set working directory to R script location
source_file_loc <- dirname(getActiveDocumentContext()$path)
setwd(source_file_loc)


###############################################################################
# 1. LOAD + CLEAN AIRTHINGS DATA   ############################################
###############################################################################
#read in data (change to location of your file)
air_dat <- read.csv("data/2960014368-latest.csv", sep = ";")

#clean raw data, deal with date/time string and change time zone to EST
air_dat_cleaned <- air_dat %>% 
  clean_names() %>%
  mutate(recorded = str_replace(recorded, "T", " "), 
         #change the time zone
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
  mutate(metric = factor(metric, levels = c("co2_ppm","pm1_mg_m3", "pm2_5_mg_m3", "voc_ppb", 
                                            "temp_f", "humidity", "pressure_h_pa", 
                                            "radon_short_term_avg_p_ci_l"),
                         labels = c("CO2 (ppm)", "PM10 (mg/m3)", "PM2.5 (mg/m3)", 
                                    "VOC (ppb)", "Temperature (F)", "Humidity (%)", 
                                    "Pressure (mbar)","Radon (pCi/L)")),
         quality_ind = case_when(metric == "CO2 (ppm)" & Result < 400 ~ "Background",
                                 metric == "CO2 (ppm)" & Result >= 400 & Result < 1000 ~ "Fair",
                                 metric == "CO2 (ppm)" & Result >= 1000 & Result < 2000 ~ "Poor",
                                 metric == "CO2 (ppm)" & Result >= 2000 & Result < 5000 ~ "High",
                                 metric == "CO2 (ppm)" & Result >= 5000  ~ "Extreme",
                                 metric %in% c("PM10 (mg/m3)", "PM2.5 (mg/m3)") & Result < 10 ~ "Good",
                                 metric %in% c("PM10 (mg/m3)", "PM2.5 (mg/m3)") & Result >= 10 & Result < 25 ~ "Fair",
                                 metric %in% c("PM10 (mg/m3)", "PM2.5 (mg/m3)") & Result >=25 ~ "Poor",
                                 metric == "VOC (ppb)" & Result < 250 ~ "Low",
                                 metric == "VOC (ppb)" & Result >= 250 & Result < 2000 ~ "Med",
                                 metric == "VOC (ppb)" & Result >= 2000 ~ "High",
                                 metric == "Radon (pCi/L)" & Result <= 2.6 ~ "Good",
                                 metric == "Radon (pCi/L)" & Result < 4 ~ "Fair",
                                 metric == "Radon (pCi/L)" & Result >= 4 ~ "Poor",
                                 metric == "Humidity (%)" & Result >= 70 ~ "High",
                                 metric == "Humidity (%)" & Result >= 60 & Result < 70 ~ "Fair",
                                 metric == "Humidity (%)" & Result >= 30 & Result < 60 ~ "Good",
                                 metric == "Humidity (%)" & Result >= 25 & Result < 30 ~ "Fair",
                                 metric == "Humidity (%)" & Result< 25 ~ "Low",
                                 TRUE ~ "none")) %>% 
  #drop first 7 days of VOC and CO2 (calibration period)
  filter(!(metric %in% c("VOC (ppb)", "CO2 (ppm)") & date < c(min(date)+days(7)))) %>% 
  group_by(metric) %>% 
  #look at the record before and after 
  mutate(rec_prior= lag(recorded, order_by = recorded, 
                        default = min(recorded)),
         rec_after = lead(recorded, order_by = recorded, 
                          default = max(recorded)),
         intrvl = interval(rec_prior, rec_after),
         dif = as.duration(intrvl)/2,
         xmin = rec_prior + dif,
         xmax = rec_after-dif,
         ymin = lag(Result, order_by = recorded,
                    default = min(Result)),
         ymax = lead(Result, order_by = recorded,
                     default = max(Result))
  ) %>% 
  group_by(metric) %>% 
  mutate(tod = cut(hour(time), breaks = c(0,12, 24), labels = c("morning", "night"), 
                   include.lowest=TRUE, right = FALSE))


air_dat_wide <- air_dat_long %>% 
  select(-recorded) %>% 
  mutate(hour_match = hour(time),
         minute_match = minute(time)) %>%
  select(metric, date, hour_match, minute_match, Result) %>% 
  pivot_wider(names_from = "metric", values_from = "Result") 



###############################################################################
# 2. SUMMARY STATS  ##########################################################
###############################################################################

tbl_summary(air_dat_wide, 
            include = c(-hour_match, -minute_match, -date), 
            missing = "no", 
            type = all_continuous() ~ "continuous2",
            statistic = all_continuous() ~ c("{N_nonmiss}",
                                             "{median} ({p25}, {p75})", 
                                             "{min} - {max}"))%>% 
  as_flex_table()

tbl_summary(air_dat_long, 
            by = metric,
            include = c(Result), 
            missing = "no", 
            type = all_continuous() ~ "continuous2",
            statistic = all_continuous() ~ c("{median} ({p25}, {p75})", 
                                             "{min} - {max}"))%>% 
  as_flex_table()



###############################################################################
# 3. VIZUALIZE ALL MEASUREMENTS   #############################################
###############################################################################

#create time series plot of all metrics  
ggplot(air_dat_long, aes(x = recorded, y = Result, color = day(recorded))) + 
  geom_line() + 
  scale_color_viridis() + 
  facet_wrap(~metric, scales = "free_y", ncol = 1) + 
  theme_pander() +
  xlab("Date")+
  ylab("Concentration") + 
  theme(legend.position = "none",
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "snow2"),
        strip.text.x = element_text(color = "#404040", face = "bold"),
        text = element_text(family = "Arial"))


#create time series plots individually 
air_plot <- list()
for(i in unique(air_dat_long$metric)){
  
  air_plot[[i]] <- ggplot(air_dat_long %>% 
                            filter(metric == i), aes(x = recorded, y = Result, color = day(recorded))) +
    geom_line() + 
    scale_color_viridis() + 
    facet_wrap(~metric, scales = "free_y") + 
    theme_pander() +
    xlab("Date")+
    ylab("Concentration") + 
    theme(legend.position = "none",
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(color = "snow2"),
          strip.text.x = element_text(color = "#404040", face = "bold"))
  
  print(air_plot[[i]])
  
}

###############################################################################
# 4. INVESTIGATE PM AND CO2  ##################################################
###############################################################################


tbl_summary(air_dat_long %>% 
              filter(metric %in% c("CO2 (ppm)", "PM10 (mg/m3)", "PM2.5 (mg/m3)")), 
            by = metric,
            include = c(Result), 
            missing = "no", 
            type = all_continuous() ~ "continuous2",
            statistic = all_continuous() ~ c("{median} ({p25}, {p75})", 
                                             "{min} - {max}"))%>% 
  as_flex_table()



#what time of day are peaks most common
air_dat_long %>% 
  filter(metric %in% c("CO2 (ppm)", "PM10 (mg/m3)", "PM2.5 (mg/m3)")) %>% 
  mutate(cat = case_when(str_detect(metric, "PM") ~ "PM",
                         TRUE ~ "CO2")) %>% 
  mutate(time = as.factor(hour(time))) %>% 
  ggplot(aes(x = time, y = Result, fill = metric)) + 
  geom_bar(stat = "summary", fun.y = "mean", position = "dodge") + 
  # geom_errorbar(stat = "summary", fun.data = mean_se, 
  #               position = "dodge") + 
  facet_wrap(~cat, scales = "free_y") + 
  viridis::scale_fill_viridis(discrete = TRUE, end = 0.75) + 
  theme_pander() +
  xlab("Hour of the day")+
  ylab("Concentration") + 
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "snow2"),
        strip.text.x = element_text(color = "#404040", face = "bold"),
        text = element_text(family = "Arial"))




###############################################################################
# 5. PROCESS INTERVENTION DATA  ###############################################
###############################################################################

cooking_log <- readxl::read_xlsx("data/cooking log.xlsx") %>% 
  mutate(meal = tolower(meal)) %>% 
  filter(!is.na(fan)) %>% 
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
         intrvl_shft_end = int_end(int_shift(intrvl, duration(hour = c(10)))),
         intrvl_shft = interval(intrvl_shft_start, intrvl_shft_end)) %>% 
  mutate(intervention = case_when(tolower(fan) == "yes" & tolower(window) == "open" ~ "fan + window",
                                  tolower(fan) == "yes" ~ "fan only",
                                  tolower(window) == "open" ~ "window only",
                                  TRUE ~ "no intervention"), 
         intervention = factor(intervention, levels = c("no intervention", 
                                                        "fan only",
                                                        "window only", 
                                                        "fan + window"))) 

cooking_plot_dat <- air_dat_long %>% 
  filter(metric %in% c("CO2 (ppm)", "PM10 (mg/m3)", "PM2.5 (mg/m3)")) %>% 
  filter(as_date(date) %in% c(cooking_log$date)) %>% 
  left_join(cooking_log, by = c("date")) %>% 
  group_by(recorded) %>% 
  mutate(keep = case_when(time >= start_shift & time <= end_shift ~ "yes",
                          TRUE ~ "no")) %>% 
  filter(keep == "yes")

###############################################################################
# 6. INTERVENTION SUMMARY STATS ###############################################
###############################################################################

cooking_summary <- cooking_log %>% 
  group_by(intervention) %>% 
  summarize(`Number of events` = n(), 
            `Avg time spent cooking` = mean(as.duration(intrvl))/60) %>% 
  bind_rows(cooking_log %>% 
              group_by(.) %>% 
              summarize(`Number of events` = n(), 
                        `Avg time spent cooking` = mean(as.duration(intrvl))/60)%>% 
              mutate(intervention = "Total")) %>% 
  rename(Intervention = intervention)


flextable::flextable(cooking_summary, cwidth = 2) %>% 
  set_caption(caption = "Summary of cooking events") 


gtsummary::tbl_summary(cooking_plot_dat, by = c(metric), include = Result, 
                       type = all_continuous() ~ "continuous2",
                       statistic = all_continuous() ~ c(
                         "{median} ({p25}, {p75})", 
                         "{min} - {max}"))%>% 
  as_flex_table()


cooking_plot_wide <- cooking_plot_dat %>% 
  ungroup() %>% 
  select(-recorded) %>% 
  mutate(hour_match = hour(time),
         minute_match = minute(time)) %>%
  select(-time, -Time) %>% 
  #pivot_wider(names_from = "metric", values_from = "Result") %>% 
  pivot_wider(names_from = "intervention", values_from = "Result") %>% 
  select(metric, `no intervention`, `fan only`, `window only`, `fan + window`, date, hour_match, minute_match) 
# select(`PM2.5 (mg/m3)`, `PM10 (mg/m3)`,`CO2 (ppm)`, intervention, date, hour_match, minute_match) 

gtsummary::tbl_summary(cooking_plot_wide, by = c(metric), 
                       include = c(`no intervention`, `fan only`, `window only`, `fan + window`), 
                       missing = "no", 
                       type = all_continuous() ~ "continuous2",
                       statistic = all_continuous() ~ c("{N_nonmiss}",
                                                        "{median} ({p25}, {p75})", 
                                                        "{min} - {max}")) %>% 
  as_flex_table()




###############################################################################
# 7. VISUALIZE INTERVENTION   #################################################
###############################################################################



cooking_plot_dat %>% 
  ggplot() + 
  geom_line(mapping = aes(x = hms::as_hms(recorded), y = Result)) + 
  geom_rect(data=cooking_log, mapping=aes(xmin = hms::as_hms(start_fmt), 
                                          xmax= hms::as_hms(end_fmt), 
                                          ymin=0, ymax=Inf, fill = intervention),
            alpha=0.5) +
  scale_fill_viridis(discrete = TRUE) +
  #scale_x_time(labels = scales::label_time(format = '%H:%M')) +
  scale_x_time(labels = scales::label_time(format = '%H')) +
  facet_grid(rows = vars(metric), cols = vars((as_date(date))), scales = "free")  +
  theme_pander() +
  xlab("Date")+
  ylab("Concentration") + 
  theme(legend.position = "top",
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "snow2"),
        strip.text.x = element_text(color = "#404040", face = "bold"),
        text = element_text(family = "Arial"))
  
ggsave("PM time series graph.png", height = 4, width = 10, units = "in")


for(i in unique(cooking_log$date)){
  
  cooking_log_date <- cooking_log %>% 
    filter(date == i)
  
  cooking_plot[[i]] <- air_dat_long %>% 
    filter(date == i) %>% 
    filter(metric %in% c("CO2 (ppm)", "PM10 (mg/m3)", "PM2.5 (mg/m3)")) %>% 
    filter(as_date(date) %in% c(cooking_log$date)) %>% 
    left_join(cooking_log, by = c("date")) %>% 
    group_by(recorded) %>% 
    mutate(keep = case_when(time >= start_shift & time <= end_shift ~ "yes",
                            TRUE ~ "no")) %>% 
    filter(keep == "yes") %>% 
    ggplot() + 
    geom_line(mapping = aes(x = hms::as_hms(recorded), y = Result)) + 
    #scale_color_viridis() +
    geom_rect(data=cooking_log, mapping=aes(xmin = hms::as_hms(start_fmt), 
                                            xmax= hms::as_hms(end_fmt), 
                                            ymin=0, ymax=Inf, fill = intervention),
              alpha=0.5) +
    facet_grid(rows = vars(metric), cols = vars((as_date(date))), scales = "free")  +
    theme_pander() +
    xlab("Date")+
    ylab("Concentration") + 
    theme(#legend.position = "none",
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(color = "snow2"),
      strip.text.x = element_text(color = "#404040", face = "bold"),
      text = element_text(family = "Arial"))
  
  
  
  print(cooking_plot[[i]])
  
  
}



time_passed <- air_dat_long %>% 
  filter(metric %in% c("CO2 (ppm)", "PM10 (mg/m3)", "PM2.5 (mg/m3)")) %>% 
  left_join(cooking_log, by = c("date")) %>% 
  group_by(recorded) %>% 
  mutate(active_cooking = case_when(time >= start & time <= end_shift ~ "yes",
                          TRUE ~ "no")) %>% 
  filter(active_cooking == "yes") %>% 
  group_by(date, metric, intervention) %>% 
  mutate(newtime = cumsum(dif)/60)


ggplot(time_passed, aes(x = newtime, y = Result, color = intervention)) + 
  geom_line() + 
  facet_grid(rows = vars(metric), cols = vars((as_date(date))), scales = "free") + 
  theme_pander() +
  scale_color_viridis(discrete = TRUE) + 
  xlab("# of minutes")+
  ylab("Concentration") + 
  theme(#legend.position = "none",
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "snow2"),
    strip.text.x = element_text(color = "#404040", face = "bold"),
    text = element_text(family = "Arial"))


ggplot(time_passed, aes(x = newtime, y = Result, group = as_date(date), color = intervention)) + 
  geom_line() + 
  facet_grid(rows = vars(metric), cols = vars(intervention), scales = "free") + 
  theme_pander() +
  xlab("# of minutes")+
  ylab("Concentration") + 
  theme(#legend.position = "none",
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "snow2"),
    strip.text.x = element_text(color = "#404040", face = "bold"),
    text = element_text(family = "Arial"))