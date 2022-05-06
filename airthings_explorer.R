### AUTHOR: AHz
### LAST EDIT: 2022-05-06
### WRITTEN IN: R version 4.0.5
### Purpose: work with AirThings data 


###############################################################################
# 0. SETUP  ##################################################################
###############################################################################

#install.packages("pacman")
library(pacman)
#p_load installs package if its not already installed, otherwise loads it

p_load(rstudioapi)
p_load(tidyverse)
p_load(lubridate)
p_load(janitor)
p_load(ggthemes)
p_load(viridis)
p_load(gtsummary)


# set working directory to R script location
source_file_loc <- dirname(getActiveDocumentContext()$path)
setwd(source_file_loc)


###############################################################################
# 1. LOAD + CLEAN DATA   #####################################################
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

#if you want to just write out the time zone corrected file, you can stop here!
write.csv(air_dat_cleaned, paste0("air_dat_cleaned_", Sys.Date() ,".csv"))


# PROCESSING STEPS FOR air_dat_long: 
#   1) Make data long so we can plot all of it at once
#   2) Clean up metrics names
#   3) Add quality indicator 
#   4) Drop the first 7 days of VOC and CO2 readings (calibration period)
#   5) Add cols to identify if the measurement is AM/PM

air_dat_long <- air_dat_cleaned %>% 
  pivot_longer(names_to = "metric",
               values_to = "Result",
               -c("recorded", "date", "time", "Time"),
               values_drop_na = TRUE) %>% 
  #rename metrics 
  mutate(metric = factor(metric, levels = c("co2_ppm","pm1_mg_m3", "pm2_5_mg_m3", "pressure_h_pa",
                                    "radon_short_term_avg_p_ci_l", "temp_f", "voc_ppb", "humidity"),
                         labels = c("CO2 (ppm)", "PM10 (mg/m3)", "PM2.5 (mg/m3)", "Pressure (mbar)",
                                    "Radon (pCi/L)", "Temperature (F)", "VOC (ppb)","Humidity (%)")),
  #add quality indicator based on airthings rules 
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
  #add a tag for morning/night
  group_by(metric) %>% 
  mutate(tod = cut(hour(time), breaks = c(0,12,24), labels = c("AM", "PM"), 
         include.lowest=TRUE, right = FALSE))

#check cut point
table(hour(air_dat_long$time), air_dat_long$tod)

#write.csv(air_dat_long, paste0("air_dat_long_", Sys.Date() ,".csv"))

###############################################################################
# 2. VIZUALIZE MEASUREMENTS   #################################################
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
          strip.text.x = element_text(color = "#404040", face = "bold"),
          text = element_text(family = "Arial"))
  
  print(air_plot[[i]])
  #click through the arrows in the "Plots" tab on the right to see each individual plot!
  #or remove the comment ("#") from this ggsave command to save them all as pngs
  ggsave(filename = paste0(make_clean_names(i), " plot.png"), height = 4, width = 10, units = "in")
  
}



###############################################################################
# 3. SUMMARIZE   ##############################################################
###############################################################################

gtsummary::tbl_summary(air_dat_long, 
                       by = metric,
                       include = c(Result), 
                       missing = "no", 
                       type = all_continuous() ~ "continuous2",
                       statistic = all_continuous() ~ c(
                         "{median} ({p25}, {p75})", 
                         "{min} - {max}"))%>% 
  as_flex_table()


# look into distributions/densities
ggplot(air_dat_long, aes(x = metric, y = Result)) + 
  ggforce::geom_sina(aes(color = metric)) + 
  geom_boxplot(width = 0.3, guides = FALSE, outlier.shape = NA, alpha = 0.5, size = 1, color = "#3a3838") + 
  facet_wrap(~metric, scales = "free") + 
  theme(legend.position = "none")


# generate density plots 
ggplot(air_dat_long, aes(x = Result)) + 
  geom_histogram(aes(y = ..density..),
                 color = "black",
                 fill = "white",
                 bins = 20) +
  geom_density(fill = "red", alpha = 0.25) + 
  facet_wrap(~metric, scales = "free")

# get summary table 
metric_summary <- air_dat_long %>% 
  group_by(metric) %>% 
  summarize(min = min(Result),
            Q1 = quantile(Result, .25),
            med = median(Result),
            mean = mean(Result),
            Q3 = quantile(Result, .75),
            max = max(Result))

