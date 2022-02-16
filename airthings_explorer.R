

library(tidyverse)
library(lubridate)
library(janitor)

# set working directory to R script location
source_file_loc <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(source_file_loc)

#read in data (change to location of your file)
air_dat <- read.csv("data/2960014368-latest.csv", sep = ";")
air_benchmarks <- read_csv("air_benchmarks.csv") 


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
  mutate(metric = factor(metric, levels = c("co2_ppm","pm1_mg_m3", "pm2_5_mg_m3", "pressure_h_pa",
                                    "radon_short_term_avg_p_ci_l", "temp_f", "voc_ppb", "humidity"),
                         labels = c("CO2 (ppm)", "PM1 (mg/m3)", "PM2.5 (mg/m3)", "Pressure (mbar)",
                                    "Radon (pCi/L)", "Temperature (F)", "VOC (ppb)","Humidity (%)")),
         quality_ind = case_when(metric == "CO2 (ppm)" & Result < 400 ~ "Background",
                                 metric == "CO2 (ppm)" & Result >= 400 & Result < 1000 ~ "Fair",
                                 metric == "CO2 (ppm)" & Result >= 1000 & Result < 2000 ~ "Poor",
                                 metric == "CO2 (ppm)" & Result >= 2000 & Result < 5000 ~ "High",
                                 metric == "CO2 (ppm)" & Result >= 5000  ~ "Extreme",
                                 metric %in% c("PM1 (mg/m3)", "PM2.5 (mg/m3)") & Result < 10 ~ "Good",
                                 metric %in% c("PM1 (mg/m3)", "PM2.5 (mg/m3)") & Result >= 10 & Result < 25 ~ "Fair",
                                 metric %in% c("PM1 (mg/m3)", "PM2.5 (mg/m3)") & Result >=25 ~ "Poor",
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
                                 metric == "Humidity (%)" & Result< 25 ~ "Low")) 




#create time series plot of all metrics  
ggplot(air_dat_long, aes(x = recorded, y = Result, color = day(recorded))) + 
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

#create time series plots individually 
air_plot <- list()
for(i in unique(air_dat_long$metric)){
  
  air_plot[[i]] <- ggplot(air_dat_long %>% 
           filter(metric == i), aes(x = recorded, y = Result, color = day(recorded))) + 
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
  
  print(air_plot[[i]])
  
}


