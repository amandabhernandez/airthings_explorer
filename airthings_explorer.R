### AUTHOR: AHz
### LAST EDIT: 2022-02-26
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
p_load(ggforce)
p_load(slider)
p_load(ggthemes)
p_load(viridis)
p_load(tidyquant)
p_load(reshape2)


# set working directory to R script location
source_file_loc <- dirname(getActiveDocumentContext()$path)
setwd(source_file_loc)


###############################################################################
# 1. LOAD + CLEAN DATA   #####################################################
###############################################################################
#read in data (change to location of your file)
air_dat <- read.csv("data/2960014368-latest.csv", sep = ";")

#air_benchmarks <- read_csv("air_benchmarks.csv") 

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

#if you want to just write out the time zone corrected file, you can stop here 
write.csv(air_dat_cleaned, paste0("air_dat_cleaned_", Sys.Date() ,".csv"))


# PROCESSING STEPS FOR air_dat_long: 
#   1) Make data long so we can plot all of it at once
#   2) Clean up metrics names
#   3) Add quality indicator 
#   4) Drop the first 7 days of VOC and CO2 readings (calibration period)
#   5) Add cols to investigate measurement before and after 
#   6) Calculate a rolling average

air_dat_long <- air_dat_cleaned %>% 
  pivot_longer(names_to = "metric",
               values_to = "Result",
               -c("recorded", "date", "time", "Time"),
               values_drop_na = TRUE) %>% 
  mutate(metric = factor(metric, levels = c("co2_ppm","pm1_mg_m3", "pm2_5_mg_m3", "pressure_h_pa",
                                    "radon_short_term_avg_p_ci_l", "temp_f", "voc_ppb", "humidity"),
                         labels = c("CO2 (ppm)", "PM10 (mg/m3)", "PM2.5 (mg/m3)", "Pressure (mbar)",
                                    "Radon (pCi/L)", "Temperature (F)", "VOC (ppb)","Humidity (%)")),
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
         dif = as.duration(intrvl)/4,
         xmin = rec_prior + dif,
         xmax = rec_after-dif,
         ymin = lag(Result, order_by = recorded,
                     default = min(Result)),
         ymax = lead(Result, order_by = recorded,
                      default = max(Result))
         ) %>% 
  #calculate a rolling average
  arrange(metric, recorded) %>% 
  group_by(metric) %>% 
  mutate(mean7day = slider::slide_index_mean(x = Result, i = recorded, before = days(6)),
         mean24hr = slider::slide_index_mean(x = Result, i = recorded, before = hours(23)),
         mean1hr = slider::slide_index_mean(x = Result, i = recorded, before = hours(1))) %>% 
  mutate(tod = cut(hour(time), breaks = c(0,12, 24), labels = c("morning", "night"), 
         include.lowest=TRUE, right = FALSE))


###############################################################################
# 2. VIZUALIZE MEASUREMENTS   #################################################
###############################################################################

#create time series plot of all metrics  
ggplot(air_dat_long, aes(x = recorded, y = Result, color = day(recorded))) + 
  geom_line() + 
  scale_color_viridis() + 
  facet_wrap(~metric, scales = "free_y") + 
  theme_pander() +
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
           filter(metric == i), aes(x = recorded, y = Result)) +
    #geom_point(aes(x = recorded, y = Result)) + 
    #geom_point(aes(color = quality_ind)) +
    geom_line() + 
    geom_line(aes(x = recorded, y = mean1hr), color = "#ec7f78", linetype = "dashed") + 
    geom_rect(aes(xmin = recorded, xmax = recorded, ymin = 0, ymax = Inf, fill = tod),
              alpha = 0.2)+
    scale_fill_manual(values = c("firebrick", "blue3")) + 
    facet_wrap(~metric, scales = "free_y") + 
    theme_pander() +
    xlab("Date")+
    ylab("Concentration") + 
    theme(panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(color = "snow2"),
          strip.text.x = element_text(color = "#556B2F", face = "bold"),
          text = element_text(family = "Arial"))
  
  print(air_plot[[i]])
  
}

###############################################################################
# 3. INVESTIGATE   #####################################################
###############################################################################

#look at low PM2.5 readings 
low_pm <- air_dat_long %>% 
  filter(str_detect(metric, "PM")) %>% 
  mutate(flag_nd = case_when(Result <=10 ~ "!",
                             TRUE ~ ""))

pm_avg <- low_pm %>% 
  group_by(metric, tod) %>% 
  summarize(pm_mean = mean(Result))


ggplot(low_pm, aes(x = recorded, y = Result)) +
  geom_path(size = 1, color = "grey")+
  geom_point(aes(color = tod), alpha = 0.4, size = 0.75) +
  #geom_hline(pm_avg, mapping = aes(yintercept = pm_mean, color = tod)) + 
  scale_color_viridis_d(name = "Time of Day", begin = 0.5, end = 0) + 
  #geom_line(aes(x = recorded, y = mean1hr), linetype = "dashed") + 
  # tidyquant::geom_ma(
  #   n = 6,           
  #   size = 1,
  #   color = "blue")+ 
  facet_wrap(~metric, scales = "free_y", ncol= 1) + 
  theme_pander() +
  xlab("Date")+
  ylab("Concentration") + 
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "snow2"),
    strip.text.x = element_text(color = "#556B2F", face = "bold"),
    text = element_text(family = "Arial"))

ggsave("PM time series graph.png", height = 4, width = 10, units = "in")

#what time of day are peaks most common
air_dat_long %>% 
  filter(str_detect(metric, "PM")) %>% 
  group_by(metric, hour(time)) %>% 
  summarize(avg = mean(Result)) %>% 
  pivot_wider(names_from = metric, values_from = avg) %>% 
  View



###############################################################################
# 4. SUMMARIZE   ##############################################################
###############################################################################

# look into distributions/densities
ggplot(air_dat_long, aes(x = metric, y = Result)) + 
  geom_sina(aes(color = metric)) + 
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

# look at correlations 
air_dt_time_match <- air_dat_long %>% 
  select(-recorded) %>% 
  mutate(hour_match = hour(time),
         minute_match = minute(time)) %>%
  select(date, metric, hour_match, minute_match, Result, mean24hr) %>% 
  pivot_wider(names_from = "metric", values_from = c("Result", "mean24hr"))


result_hr_avg <- air_dt_time_match %>% 
  group_by(date, hour_match) %>% 
  summarize(across(.cols = contains("Result"), .fns = mean, na.rm = TRUE))

cor.matrix_result <- cor(air_dt_time_match[, 3:11], method = "spearman",
                        use = "complete.obs")

cor.matrix_result_hravg <- cor(result_hr_avg[,3:10], method = "spearman",
                         use = "complete.obs")

cor.matrix_roavg <- cor(air_dt_time_match[, 12:19], method = "spearman",
                        use = "complete.obs")

get_cor <- function(dat){
  
  dat[lower.tri(dat)]<- NA
  
  reshape2::melt(dat) %>%
    ggplot(aes(x=Var1, y=Var2, fill=value)) + 
    geom_tile() + 
    geom_text(aes(label = round(value,2))) + 
    scale_fill_gradient2(limit = c(-1,1), breaks = c(-1, -.75 ,-.5, -.25, 0, .25,.5, .75, 1), 
                         low = "#29af7f", high =  "#b8de29", mid = "white", 
                         name = "Cor value") + 
    scale_x_discrete(position = "top") +
    theme(panel.background = element_rect(fill = "white"),
          axis.text.y = element_text(size=12),
          axis.title.x = element_text(size=14),
          axis.title.y = element_text(size=14),
          legend.position = "none") +
    xlab("")+
    ylab("")
}


# corr matrix for all raw results with direct matches (ex: must have CO2 and PM measurement at the same time)
get_cor(cor.matrix_result)

#corr matrix for an hourly average of all measurements (to deal with intermittent testing )
get_cor(cor.matrix_result_hravg)

#corr matrix using rolling 24 hr averages
get_cor(cor.matrix_roavg)

