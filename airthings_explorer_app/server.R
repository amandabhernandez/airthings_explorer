#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(lubridate)

# load and clean data

air_dat <- read.csv("../data/2960014368-latest.csv", sep = ";")


air_dat_cleaned <- air_dat %>% 
    clean_names() %>%
    separate(recorded, sep = "T", into = c("date", "time"), remove = FALSE) %>% 
    mutate(date = ymd(date),
           time = hms(time),
           Time = ifelse(minute(time) >= 10, paste0(hour(time), ":", minute(time)),
                         paste0(hour(time), ":0", minute(time))),
           recorded = ymd_hms(recorded))


air_dat_long <- air_dat_cleaned %>% 
    pivot_longer(names_to = "metric",
                 values_to = "Result",
                 -c("recorded", "date", "time", "Time"),
                 values_drop_na = TRUE) %>% 
    mutate(metric = case_when(metric == "co2_ppm" ~ "CO2 (ppm)",
                              metric == "pm1_mg_m3" ~ "PM1 (mg/m3)",
                              metric == "pm2_5_mg_m3" ~ "PM2.5 (mg/m3)",
                              metric == "pressure_h_pa" ~ "Pressure (mbar)",
                              metric == "radon_short_term_avg_p_ci_l" ~ "Radon (pCi/L)",
                              metric == "temp_f" ~ "Temperature (F)",
                              metric == "voc_ppb" ~ "VOC (ppb)",
                              metric == "humidity" ~ "Humidity (%)"))


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    output$date_range <- renderUI({
        dateRangeInput("date_range", "Date Range:", 
                       start = min(air_dat_long$date),
                       end = max(air_dat_long$date),
                       min = min(air_dat_long$date),
                       max = max(air_dat_long$date))
    })
    
    output$metric <- renderUI({
        pickerInput("metric", "Select Metric", 
                       choices = as.list(unique(air_dat_long$metric)),
                       options = list(`actions-box` = TRUE,
                                      title = "Select metric(s)"),
                       multiple = TRUE
                       )
    })
    
    #filter data based on user input
    dat <- reactive(
        air_dat_long %>% 
            filter(date >= min(input$date_range) &
                       date  <= max(input$date_range)) %>% 
            filter(metric %in% c(input$metric))
        
    )
    
    
    output$airplot <- renderPlotly({
        req(input$metric)
        show_dat <- dat()
        
        air_plot <- ggplot(show_dat) + 
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
        
        #print figure
        ggplotly(air_plot, 
                 tooltip = c("y", "label", "label2"),
                 height = length(unique(show_dat$metric))*300) 

})

})
