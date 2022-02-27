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
#air_dat <- read.csv(url("https://airthings-prod-user-data.s3.amazonaws.com/0536b1ff-53fc-4275-82a0-bae0ea638fa2/csv-exports/2960014368-latest.csv?X-Amz-Security-Token=IQoJb3JpZ2luX2VjEPj%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEaCXVzLWVhc3QtMSJHMEUCIQDbTa5jvtuRlU2puxH5b6whX0SSngcRllLQes7kka6TFgIgWKV%2FMu9JfW7%2F0B8cA%2BWq4TAQBhHTvvnJpXoU8L2huu0qkgIIURADGgw0MTI5MzIyMzk1NDMiDD3DriRAAyxE%2FwdARyrvAXboYTc2lbklrULZzWp7yk86EhPa84fDg%2BKJShzQ0h4kgRkS67afD2X%2FERelga%2B%2BvDeKRplYYpk2pzcs5y16gwmyUr7KmuJnT4fSw%2FQOO7eVXsn%2FPhm%2BTalPHJ15LMbk7WABwMsQD2G1AFh1C7PxsuJGYnDc1cdrfja0xX3tSBuU5Bx2o8Q1D7ZpOpcpJsGYrEpR2%2FPp1iKef0DyWPhNO9kjhrHAB0sJeXfrt6FLUjSbfyIxPgONpeWD7xxPQNPMNNQilHeCCrjgTNEzUzWJf0g4GDneuvyh2VXNf0QIhSceSwoUkzx%2FnpHrR0S1RCl8MI3L0JAGOpoBsGupNFDETRqfl8qEvh5d6noHEUenl%2BANrY1KG6jDMAZmJSBAxMZC4NnmxdJsIKnCtTxUn1qXky0LSbND%2B8VUWRfyXRF5bwNi%2BsKF8Ywub9GSFWUGt8TKamPwfD164XTKslOarKqK5GV8%2Bynl7YB6ss0NUK0P3jDzZg9YJwI4lbjSTvznpXJGg62iX3BfgzdSvkFqirswx8b7dw%3D%3D&X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Date=20220222T002005Z&X-Amz-SignedHeaders=host&X-Amz-Expires=1799&X-Amz-Credential=ASIAWAJFN6C35BFJ3DPK%2F20220222%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Signature=7040b8be8b7088186e621729ba831035a52a6a9ae5d81ed415ef3f74474615ac"), sep = ";")



air_dat_cleaned <- air_dat %>% 
  clean_names() %>%
  mutate(recorded = str_replace(recorded, "T", " "), 
         recorded = with_tz(recorded, tz = "America/New_York")) %>% 
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
                            metric == "pm1_mg_m3" ~ "PM10 (mg/m3)",
                            metric == "pm2_5_mg_m3" ~ "PM2.5 (mg/m3)",
                            metric == "pressure_h_pa" ~ "Pressure (mbar)",
                            metric == "radon_short_term_avg_p_ci_l" ~ "Radon (pCi/L)",
                            metric == "temp_f" ~ "Temperature (F)",
                            metric == "voc_ppb" ~ "VOC (ppb)",
                            metric == "humidity" ~ "Humidity (%)")) %>%  
  #drop first 7 days of VOC and CO2 (calibration period)
  filter(!(metric %in% c("VOC (ppb)", "CO2 (ppm)") & date < c(min(date)+days(7))))


table(air_dat_long$metric, air_dat_long$date)


#client_id <- ""
#client_secret <- ""

client_id <- "89cbf1d1-dcf6-4dd5-9d64-509f4f5b860e"
client_secret <- "7212b91f-b960-4c70-b1cb-4446d047dda6"

app <- httr::oauth_app(appname = "airthings_AH_test",
                       key = client_id,
                       secret = client_secret,
                       redirect_uri = "")

endpoint <-  httr::oauth_endpoint(
  request = NULL,
  authorize = "https://accounts.airthings.com/authorize",
  access = "https://accounts-api.airthings.com/v1/token")

token <- httr::oauth2.0_token(endpoint = endpoint,
                              app = app,
                              client_credentials = TRUE,
                              scope = "read:device:current_values",
                              use_basic_auth = TRUE, cache = FALSE)


request <- httr::GET("https://ext-api.airthings.com/v1/devices/2960014368/latest-samples",
                     accept_json(),
                     #add_headers(Authorization = paste("Bearer", client_secret, sep = " ")),
                     httr::config(token = token))

raw_current <- data.frame(content(request, "parsed")) 

names(raw_current) <- c(str_remove(names(raw_current), "data."))

current_dat <- raw_current %>% 
  mutate(time = with_tz(as_datetime(time), tz = "America/New_York"),
         temp =  (9/5) * temp + 32) #%>% 
  # select(-relayDeviceType) %>% 
  # pivot_longer(names_to = "metric", values_to = "result",-time)

#set boundaries for current measurement rules

get_avg.med <- air_dat_long %>% 
  group_by(metric) %>% 
  summarize(med = median(Result),
            avg = mean(Result), 
            Q3 = quantile(Result, .75),
            Q1 = quantile(Result, .25))

#wide data for the XY comparison plots 
air_dt_time_match <- air_dat_long %>% 
  select(-recorded) %>% 
  mutate(hour_match = hour(time),
         minute_match = minute(time)) %>%
  select(-time, -Time) %>% 
  pivot_wider(names_from = "metric", values_from = "Result")



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
    geom_boxplot(aes(x = metric, y = Result)) + 
    # geom_boxplot(air_dat_long %>%
    #                filter(metric == facet), aes(x = metric, y = Result )) +-
    ggthemes::theme_pander() +
    theme(legend.position = "none",
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(color = "snow2"), 
          axis.text.x = element_blank(),
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
  
  new_dat <- ggdat %>% 
    select(-recorded) %>% 
    mutate(hour_match = hour(time),
           minute_match = minute(time)) %>%
    select(-time, -Time) %>% 
    pivot_wider(names_from = "metric", values_from = "Result") %>% 
    select(`facet`, input, date, hour_match, minute_match) %>% 
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


