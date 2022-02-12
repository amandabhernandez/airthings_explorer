

library(tidyverse)
library(lubridate)
library(janitor)

# set working directory to R script location
source_file_loc <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(source_file_loc)

#read in data
air_dat <- read.csv("data/2960014368-latest.csv", sep = ";")

#clean raw data, deal with date/time string
air_dat_cleaned <- air_dat %>% 
  clean_names() %>%
  separate(recorded, sep = "T", into = c("date", "time"), remove = FALSE) %>% 
  mutate(date = ymd(date),
         #time = hms(time),
         Time = paste0(hour(hms(time)), ":", minute(hms(time))),
         recorded = ymd_hms(recorded)) %>% 
  select(-time)

#make data long so we can plot it all at once 
air_dat_long <- air_dat_cleaned %>% 
  pivot_longer(names_to = "metric",
               values_to = "Result",
               -c("recorded", "date", "Time"),
               values_drop_na = TRUE) %>% 
  mutate(metric = case_when(metric == "co2_ppm" ~ "CO2 (ppm)",
                            metric == "pm1_mg_m3" ~ "PM1 (mg/m3)",
                            metric == "pm2_5_mg_m3" ~ "PM2.5 (mg/m3)",
                            metric == "pressure_h_pa" ~ "Pressure (mbar)",
                            metric == "radon_short_term_avg_p_ci_l" ~ "Radon (pCi/L)",
                            metric == "temp_f" ~ "Temperature (F)",
                            metric == "voc_ppb" ~ "VOC (ppb)",
                            metric == "humidity" ~ "Humidity (%)"))

#create time series plot of all metrics  
ggplot(air_dat_long, aes(x = recorded, y = Result, color = day(recorded))) + 
  #geom_point() +
  geom_line() + 
  viridis::scale_color_viridis() + 
  facet_wrap(~metric, scales = "free_y") + 
  ggthemes::theme_pander() +
  xlab("Date")+
  ylab("Concentration") + 
  theme(legend.position = "none",
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "snow2"),
        strip.text.x = element_text(color = "#556B2F", face = "bold"),
        text = element_text(family = "Arial"))

