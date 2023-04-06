### AUTHOR: AHz
### LAST EDIT: 2023-04-06
### WRITTEN IN: R version 4.2.2
### Purpose: clean raw airthings data


###############################################################################
# 0. SETUP  ###################################################################
###############################################################################


#install.packages("pacman")
pacman::p_load(tidyverse, lubridate, janitor)

# set working directory to R script location
source_file_loc <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(source_file_loc)


###############################################################################
# 1. LOAD + CLEAN AIRTHINGS DATA   ############################################
###############################################################################

#read in data
air_dat <- read.csv("data/2960014368-latest.csv", sep = ";")

#clean raw data, deal with date/time string and change time zone to EST
air_dat_cleaned <- air_dat %>% 
  janitor::clean_names() %>%
  mutate(recorded = str_replace(recorded, "T", " "), 
         #change the time zone
         recorded = case_when(str_detect(recorded, "\\d*:\\d*:") ~ 
                                with_tz(ymd_hms(recorded), tz = "America/New_York"),
                              str_detect(recorded, "(?<!:)\\d{2}:\\d{2}$") ~ 
                                with_tz(ymd_hm(recorded), tz = "America/New_York"))
  ) %>% 
  #drop seconds from measurement
  mutate(recorded = format(recorded, format='%Y-%m-%d %H:%M')) %>% 
  separate(recorded, sep = " ", into = c("date", "time"), remove = FALSE) %>% 
  mutate(date = ymd(date),
         time = hm(time)) 


air_dat_long <- air_dat_cleaned %>% 
  pivot_longer(names_to = "metric",
               values_to = "Result",
               -c("recorded", "date", "time"),
               values_drop_na = TRUE) %>% 
  mutate(metric = factor(metric, levels = c("co2_ppm","pm1_mg_m3", "pm2_5_mg_m3", 
                                            "voc_ppb", "temp_f", "humidity", 
                                            "pressure_h_pa", "radon_short_term_avg_p_ci_l"),
                         labels = c("CO2 (ppm)", "PM10 (ug/m3)", "PM2.5 (ug/m3)", 
                                    "VOC (ppb)", "Temperature (F)", "Humidity (%)", 
                                    "Pressure (mbar)","Radon (pCi/L)"))) %>% 
  #drop first 7 days of VOC and CO2 (calibration period)
  filter(!(metric %in% c("VOC (ppb)", "CO2 (ppm)") & date < c(min(date)+days(7))))


#make data wide again with measurements matching by hour + minute
air_dat_wide <- air_dat_long %>% 
  pivot_wider(names_from = "metric", values_from = "Result") 
