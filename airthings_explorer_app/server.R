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
                filter(recorded >= max(air_dat_long$recorded)-hours(12))
        }
        
        else if(input$date_select == "24hr"){
            air_dat_long %>%
                filter(recorded >= max(air_dat_long$recorded) - hours(24))
        }
        else if(input$date_select == "36hr"){
            air_dat_long %>%
                filter(recorded >= max(air_dat_long$recorded) - hours(36))
        }
        else if(input$date_select == "48hr"){
            air_dat_long %>%
                filter(recorded >= max(air_dat_long$recorded) - hours(48))
        }
        else if(input$date_select == "1wk"){
            air_dat_long %>%
                filter(recorded >= max(air_dat_long$recorded)- weeks(1))
        }
        else if(input$date_select == "1mo"){
            air_dat_long %>%
                filter(recorded >= max(air_dat_long$recorded) - months(1))
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
            title = "Radon (short term avg)",
            icon = fontawesome::fa_i("radiation"), 
            color = "olive"
            # color = ifelse(current_dat$radonShortTermAvg > get_avg.med$Q3[which(get_avg.med$metric == 
            #                                                            "Radon (pCi/L)")], "yellow",
            #                ifelse(current_dat$radonShortTermAvg < get_avg.med$Q1[which(get_avg.med$metric == 
            #                                                                   "Radon (pCi/L)")],
            #                       "light-blue", "olive")),
            # subtitle = ifelse(current_dat$radonShortTermAvg > get_avg.med$Q3[which(get_avg.med$metric == 
            #                                                               "Radon (pCi/L)")], "this is higher than usual",
            #                   ifelse(current_dat$radonShortTermAvg < get_avg.med$Q1[which(get_avg.med$metric == 
            #                                                                      "Radon (pCi/L)")],
            #                          "this is lower than usual", ""))
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
        
        air_plot <- ggplot(show_dat) %>%
            timeseries_plot()

        #print figure
        ggplotly(air_plot,
                 tooltip = c("y", "label", "label2"),
                 height = length(unique(show_dat$metric))*300)
    })
    
    
    #temperature page 
    output$tempplot <- renderPlotly({
        time_subplot(dat(), "Temperature (F)")

        
    })
    output$temp_density <- renderPlotly({
        dens_plotly(dat(), "Temperature (F)")
    })
    
    output$temp_xy <- renderPlotly({
        req(input$temp_y_comp)
        comp_xy(dat(), "Temperature (F)", input$temp_y_comp)
        
    })
    
    #humidity page 
    output$humplot <- renderPlotly({
        time_subplot(dat(), "Humidity (%)")
        
    })
    output$hum_density <- renderPlotly({
        dens_plotly(dat(), "Humidity (%)")
    })
    
    output$hum_xy <- renderPlotly({
        req(input$hum_y_comp)
        
        comp_xy(dat(), "Humidity (%)", input$hum_y_comp)
        
    })
    
    #CO2 page 
    output$pressplot <- renderPlotly({
        time_subplot(dat(), "Pressure (mbar)")
        
        
    })
    output$pressure_density <- renderPlotly({
    dens_plotly(dat(), "Pressure (mbar)")

    })
    
    output$press_xy <- renderPlotly({
        req(input$press_y_comp)
        comp_xy(dat(), "Pressure (mbar)", input$press_y_comp)
        
    })
    
    #CO2 page 
    output$co2plot <- renderPlotly({
        time_subplot(dat(), "CO2 (ppm)")
        
        
    })
    output$co2_density <- renderPlotly({
        dens_plotly(dat(), "CO2 (ppm)")
        
    })
    
    output$co2_xy <- renderPlotly({
        req(input$co2_y_comp)
        comp_xy(dat(), "CO2 (ppm)", input$co2_y_comp)
        
    })
    #VOC page 
    output$vocplot <- renderPlotly({
        time_subplot(dat(), "VOC (ppb)")
        
        
    })
    output$voc_density <- renderPlotly({
        dens_plotly(dat(), "VOC (ppb)")
        
    })
    
    output$voc_xy <- renderPlotly({
        req(input$voc_y_comp)
        comp_xy(dat(), "VOC (ppb)", input$voc_y_comp)
        
    })
    
    #PM page
    output$pmplot <- renderPlotly({
        sub_dat <- dat() %>% 
            filter(str_detect(metric, "PM"))
        
        sub_plot <- ggplot(sub_dat) +
            geom_line(aes(x = recorded, y = Result ,
                          color = metric,
                          alpha = metric, 
                          label = Time,
                          label2 = date)) +
            scale_color_manual(values = c("#cf9c2e", "#ec7f78"))+
            scale_alpha_manual(values = c(0.75, .5))+
            #viridis::scale_color_viridis() +
            #facet_wrap(~metric, ncol = 1, scales = "free_y") +
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
        
        # sub_violin_ly <- chronicle::make_violin(sub_dat, value = 'Result', 
        #                                         plot_palette = c("#cf9c2e", "#ec7f78"), 
        #                                         groups = 'metric')
        
        
        sub_box <- ggplot(sub_dat) + 
            geom_boxplot(aes(x = metric, y = Result, fill = metric)) + 
            scale_fill_manual(values = c("#cf9c2e", "#ec7f78"))+
            # geom_boxplot(air_dat_long %>%
            #                filter(metric == facet), aes(x = metric, y = Result )) +-
            ggthemes::theme_pander() +
            theme(legend.position = "none",
                  panel.grid.major.y = element_blank(),
                  panel.grid.major.x = element_line(color = "snow2"), 
                  text = element_text(family = "Arial"))
        
        sub_box_ly <- ggplotly(sub_box)
        
        subplot(list(sub_plotly, sub_box_ly), widths = c(.9, .1), 
                nrows = 1, shareY = TRUE, margin = 0)
        
        
    })
    output$pm_density <- renderPlotly({
        dens_plotly(dat(), c("PM2.5 (mg/m3)", "PM10 (mg/m3)"))
        
    })
    
    output$pm_xy <- renderPlotly({
        req(input$pm_y_comp)
        
        new_dat <- dat() %>%
            select(-recorded) %>%
            mutate(hour_match = hour(time),
                   minute_match = minute(time)) %>%
            select(-time, -Time) %>%
            pivot_wider(names_from = "metric", values_from = "Result") %>%
            select(`PM2.5 (mg/m3)`,`PM10 (mg/m3)`, input$pm_y_comp, date, hour_match, minute_match) %>%
            rename(y = input$pm_y_comp)
        
        pm <- highlight_key(new_dat)
        if(!input$pm_y_comp %in% c(c("PM2.5 (mg/m3)", "PM10 (mg/m3)")) ){
            pm2.5 <- plot_ly(pm, x = ~`PM2.5 (mg/m3)`, y = ~y) %>%
                add_markers(showlegend = FALSE) %>% 
                highlight("plotly_selected")
            pm10 <- plot_ly(pm, x = ~`PM10 (mg/m3)`, y = ~y) %>%
                add_markers(showlegend = FALSE) %>% 
                highlight("plotly_selected")
        }
        else if(input$pm_y_comp == "PM2.5 (mg/m3)"){
            pm2.5 <- plot_ly(pm, x = ~y, y = ~y) %>%
                add_markers(showlegend = FALSE) %>% 
                highlight("plotly_selected")
            pm10 <- plot_ly(pm, x = ~`PM10 (mg/m3)`, y = ~y) %>%
                add_markers(showlegend = FALSE) %>% 
                highlight("plotly_selected")
        }
        else if(input$pm_y_comp == "PM10 (mg/m3)"){
            pm2.5 <- plot_ly(pm, x = ~`PM2.5 (mg/m3)`, y = ~y) %>%
                add_markers(showlegend = FALSE) %>% 
                highlight("plotly_selected")
            pm10 <- plot_ly(pm, x = ~y, y = ~y) %>%
                add_markers(showlegend = FALSE) %>% 
                highlight("plotly_selected")
        }

        
        subplot(list(pm2.5, pm10), titleX = TRUE)
        
        #bscols(width = c(2,2), pm2.5, pm10)
        
    })
    
    
    
})
