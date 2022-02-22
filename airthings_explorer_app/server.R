#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

shinyServer(function(input, output) {
    
    # output$date_range <- renderUI({
    #     dateRangeInput("date_range", "Date Range:", 
    #                    start = min(air_dat_long$date),
    #                    end = max(air_dat_long$date),
    #                    min = min(air_dat_long$date),
    #                    max = max(air_dat_long$date))
    # })
    
    # output$metric <- renderUI({
    #     pickerInput("metric", "Select Metric", 
    #                 choices = as.list(unique(air_dat_long$metric)),
    #                 options = list(`actions-box` = TRUE,
    #                                title = "Select metric(s)"),
    #                 multiple = TRUE
    #     )
    # })
    
    #filter data based on user input
    dat <- eventReactive(input$date_select, {
    #reactive({
        req(input$date_select)
        if(input$date_select == "Date Range"){
            air_dat_long %>% 
                filter(date >= min(input$date_range) &
                           date  <= max(input$date_range))  
        }
        
        else if(input$date_select == "12hr"){
            air_dat_long %>%
                filter(recorded >= now()-hours(12))
        }
        
        else if(input$date_select == "24hr"){
            air_dat_long %>%
                filter(recorded >= now() - hours(24))
        }
        else if(input$date_select == "36hr"){
            air_dat_long %>%
                filter(recorded >= now() - hours(36))
        }
        else if(input$date_select == "48hr"){
            air_dat_long %>%
                filter(recorded >= now() - hours(48))
        }
        else if(input$date_select == "1wk"){
            air_dat_long %>%
                filter(recorded >= now() - weeks(1))
        }
        else if(input$date_select == "1mo"){
            air_dat_long %>%
                filter(recorded >= now() - months(1))
        }
        else{
            air_dat_long
        }
        # else{
        #     air_dat_long
        # }
        
        #dat_date
        #print(nrow(dat_date))
        #print(input$date_select)
        
        
    #})
        })
    
    # icons from https://fontawesome.com/v4/icons/
    output$temp_current <- renderInfoBox({
        infoBox(
            value = paste0(current_dat$temp, "°F"),
            title = "Temperature",
            icon = icon("thermometer-half"),
            color = ifelse(current_dat$temp > get_avg.med$Q3[which(get_avg.med$metric == 
                                                                        "Temperature (F)")], "yellow",
                           ifelse(current_dat$temp < get_avg.med$Q1[which(get_avg.med$metric == 
                                                                              "Temperature (F)")],
                                  "light-blue", "olive")),
            subtitle = ifelse(current_dat$temp > get_avg.med$Q3[which(get_avg.med$metric == 
                                                                          "Temperature (F)")], "it's warmer than usual",
                              ifelse(current_dat$temp < get_avg.med$Q1[which(get_avg.med$metric == 
                                                                                 "Temperature (F)")],
                                     "it's colder than usual", ""))
        )
    })
    
    output$humidity_current <- renderInfoBox({
        infoBox(
            value = paste0(current_dat$humidity, "%"),
            title = "Humidity",
            icon = icon("tint"), 
            color = ifelse(current_dat$humidity > get_avg.med$Q3[which(get_avg.med$metric == 
                                                                       "Humidity (%)")], "yellow",
                           ifelse(current_dat$humidity < get_avg.med$Q1[which(get_avg.med$metric == 
                                                                              "Humidity (%)")],
                                  "light-blue", "olive")),
            subtitle = ifelse(current_dat$humidity > get_avg.med$Q3[which(get_avg.med$metric == 
                                                                          "Humidity (%)")], "it's more humid than usual",
                              ifelse(current_dat$humidity < get_avg.med$Q1[which(get_avg.med$metric == 
                                                                                 "Humidity (%)")],
                                     "it's less humid than usual", ""))
        )
    })
    
    output$pressure_current <- renderInfoBox({
        infoBox(
            value = paste0(current_dat$pressure, " hPa"),
            title = "Pressure",
            icon = icon("tachometer"), 
            color = ifelse(current_dat$pressure > get_avg.med$Q3[which(get_avg.med$metric == 
                                                                       "Pressure (mbar)")], "yellow",
                           ifelse(current_dat$pressure < get_avg.med$Q1[which(get_avg.med$metric == 
                                                                              "Pressure (mbar)")],
                                  "light-blue", "olive")),
            subtitle = ifelse(current_dat$pressure > get_avg.med$Q3[which(get_avg.med$metric == 
                                                                          "Pressure (mbar)")], "the pressure is higher than usual",
                              ifelse(current_dat$pressure < get_avg.med$Q1[which(get_avg.med$metric == 
                                                                                 "Pressure (mbar)")],
                                     "the pressure is lower than usual", ""))
        )
    })
    output$radon_current <- renderInfoBox({
        infoBox(
            value = paste0(current_dat$radonShortTermAvg, " pCi/L"),
            title = "Radon",
            icon = fontawesome::fa_i("radiation"), 
            color = ifelse(current_dat$radonShortTermAvg > get_avg.med$Q3[which(get_avg.med$metric == 
                                                                       "Radon (pCi/L)")], "yellow",
                           ifelse(current_dat$radonShortTermAvg < get_avg.med$Q1[which(get_avg.med$metric == 
                                                                              "Radon (pCi/L)")],
                                  "light-blue", "olive")),
            subtitle = ifelse(current_dat$radonShortTermAvg > get_avg.med$Q3[which(get_avg.med$metric == 
                                                                          "Radon (pCi/L)")], "this is higher than usual",
                              ifelse(current_dat$radonShortTermAvg < get_avg.med$Q1[which(get_avg.med$metric == 
                                                                                 "Radon (pCi/L)")],
                                     "this is lower than usual", ""))
        )
    })
    output$co2_current <- renderInfoBox({
        infoBox(
            value = paste0(current_dat$co2, " ppm"),
            title = "CO2",
            icon = icon("cloud"), 
            color = ifelse(current_dat$co2 > get_avg.med$Q3[which(get_avg.med$metric == 
                                                                       "CO2 (ppm)")], "yellow",
                           ifelse(current_dat$co2 < get_avg.med$Q1[which(get_avg.med$metric == 
                                                                              "CO2 (ppm)")],
                                  "light-blue", "olive")),
            subtitle = ifelse(current_dat$co2 > get_avg.med$Q3[which(get_avg.med$metric == 
                                                                          "CO2 (ppm)")], "this is higher than usual",
                              ifelse(current_dat$co2 < get_avg.med$Q1[which(get_avg.med$metric == 
                                                                                 "CO2 (ppm)")],
                                     "this is lower than usual", ""))
        )
    })
    output$voc_current <- renderInfoBox({
        infoBox(
            value = paste0(current_dat$voc, " ppb"),
            title = "VOCs",
            icon = fontawesome::fa_i("wind"), 
            color = ifelse(current_dat$voc > get_avg.med$Q3[which(get_avg.med$metric == 
                                                                       "VOC (ppb)")], "yellow",
                           ifelse(current_dat$voc < get_avg.med$Q1[which(get_avg.med$metric == 
                                                                              "VOC (ppb)")],
                                  "light-blue", "olive")),
            subtitle = ifelse(current_dat$voc > get_avg.med$Q3[which(get_avg.med$metric == 
                                                                          "VOC (ppb)")], "this is higher than usual",
                              ifelse(current_dat$voc < get_avg.med$Q1[which(get_avg.med$metric == 
                                                                                 "VOC (ppb)")],
                                     "this is lower than usual", ""))
        )
    })
    output$pm2.5_current <- renderInfoBox({
        infoBox(
            value = paste0(current_dat$pm25, " mg/m3"),
            title = "PM 2.5",
            icon = fontawesome::fa_i("smog"), 
            color = ifelse(current_dat$pm25 > get_avg.med$Q3[which(get_avg.med$metric == 
                                                                       "PM2.5 (mg/m3)")], "yellow",
                           ifelse(current_dat$pm25 < get_avg.med$Q1[which(get_avg.med$metric == 
                                                                              "PM2.5 (mg/m3)")],
                                  "light-blue", "olive")),
            subtitle = ifelse(current_dat$pm25 > get_avg.med$Q3[which(get_avg.med$metric == 
                                                                          "PM2.5 (mg/m3)")], "this is higher than usual",
                              ifelse(current_dat$pm25 < get_avg.med$Q1[which(get_avg.med$metric == 
                                                                                 "PM2.5 (mg/m3)")],
                                     "this is lower than usual", ""))
        )
    })
    output$pm10_current <- renderInfoBox({
        infoBox(
            value = paste0(current_dat$pm1, " mg/m3"),
            title = "PM 10",
            icon = icon("smog"), 
            color = ifelse(current_dat$pm1 > get_avg.med$Q3[which(get_avg.med$metric == 
                                                                       "PM10 (mg/m3)")], "yellow",
                           ifelse(current_dat$pm1 < get_avg.med$Q1[which(get_avg.med$metric == 
                                                                              "PM10 (mg/m3)")],
                                  "light-blue", "olive")),
            subtitle = ifelse(current_dat$pm1 > get_avg.med$Q3[which(get_avg.med$metric == 
                                                                          "PM10 (mg/m3)")], "this is higher than usual",
                              ifelse(current_dat$pm1 < get_avg.med$Q1[which(get_avg.med$metric == 
                                                                                 "PM10 (mg/m3)")],
                                     "this is lower than usual", ""))
        )
    })
    
    
    
#     output$airplot <- renderPlotly({
#         req(input$metric)
#         show_dat <- dat()
#     
#         air_plot <- list()
#         air_plot_names <- list()
#         for(i in unique(show_dat$metric)){
#             
#             air_plot2 <- ggplot(show_dat %>% 
#                                         filter(metric == i), 
#                                     aes(x = recorded, y = Result, color = day(recorded))) + 
#                 geom_line() + 
#                 viridis::scale_color_viridis() + 
#                 facet_wrap(~metric, scales = "free_y") + 
#                 ggthemes::theme_pander() +
#                 xlab("Date")+
#                 ylab("Concentration") + 
#                 theme(legend.position = "none",
#                       panel.grid.major.y = element_blank(),
#                       panel.grid.major.x = element_line(color = "snow2"),
#                       strip.text.x = element_text(color = "#556B2F", face = "bold"),
#                       text = element_text(family = "Arial"))
#             
#                 air_plot[[i]] <-  ggplotly(air_plot2, 
#                                            tooltip = c("y", "label", "label2"),
#                                            height = 300*length(unique(show_dat$metric))) 
# 
# 
#             
#         }
#         
#         
#         
#         subplot(c(air_plot), nrows = length(unique(show_dat$metric)))
# 
# })
    
    
    output$airplot <- renderPlotly({
        show_dat <- dat()
        
        air_plot <- ggplot(dat()) +
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
