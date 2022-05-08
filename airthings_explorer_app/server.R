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
  
  output$metric <- renderUI({
    pickerInput("metric", "Select Metric",
                choices = as.list(unique(air_dat_long$metric)), 
                selected = c(unique(air_dat_long$metric)),
                options = list(`actions-box` = TRUE,
                               title = "Select metric(s)"),
                multiple = TRUE
    )
  })
  
  # output$roavg <- renderUI({
  #   pickerInput("roavg", "Add a rolling average",
  #               choices = as.list(c("mean1hr", "mean24hr", "mean7day")),
  #               options = list(`actions-box` = TRUE,
  #                              title = "Select averaging period(s)"),
  #               #selected = "none",
  #               multiple = TRUE)
  # })
  
  #filter data based on user input
  dat <- eventReactive(input$date_select, {
    req(input$date_select)
    
    if(input$date_select == "dates"){
      air_dat_long %>% 
        filter(date >= min(input$date_range) &
                 date  <= max(input$date_range)) #%>% 
        # arrange(metric, recorded) %>% 
        # group_by(metric) %>% 
        # mutate(mean7day = slider::slide_index_mean(x = Result, i = recorded, before = days(6)),
        #        mean24hr = slider::slide_index_mean(x = Result, i = recorded, before = hours(23)),
        #        mean1hr = slider::slide_index_mean(x = Result, i = recorded, before = hours(1)))
    }
    
    else if(input$date_select == "12hr"){
      dat_12hr
    }
    
    else if(input$date_select == "24hr"){
      dat_24hr
    }
    else if(input$date_select == "36hr"){
      dat_36hr
    }
    else if(input$date_select == "48hr"){
      dat_48hr
    }
    else if(input$date_select == "1wk"){
      dat_1wk
    }
    else if(input$date_select == "1mo"){
      dat_1mo
    }
    else{
      air_dat_long
    }
    
    
    
  })
  
  
  current_dat <- reactive(dat() %>% 
    group_by(metric) %>% 
    summarize(med = median(Result)) %>% 
    pivot_wider(names_from = "metric", values_from = "med"))
  
  
  # icons from https://fontawesome.com/v4/icons/
  output$temp_current <- renderInfoBox({
    current_dat_box <- current_dat()
    infoBox(
      value = paste0(current_dat_box$`Temperature (F)`, "°F"),
      title = "Temperature",
      icon = icon("thermometer-half"),
      color = "blue"
      # color = ifelse(current_dat$temp > get_avg.med$Q3[which(get_avg.med$metric == 
      #                                                          "Temperature (F)")], "orange",
      #                ifelse(current_dat$temp < get_avg.med$Q1[which(get_avg.med$metric == 
      #                                                                 "Temperature (F)")],
      #                       "teal", "olive")),
      # subtitle = ifelse(current_dat$temp > get_avg.med$Q3[which(get_avg.med$metric == 
      #                                                             "Temperature (F)")], "it's warmer than usual",
      #                   ifelse(current_dat$temp < get_avg.med$Q1[which(get_avg.med$metric == 
      #                                                                    "Temperature (F)")],
      #                          "it's colder than usual", ""))
    )
  })
  
  output$humidity_current <- renderInfoBox({
    current_dat_box <- current_dat()
    infoBox(
      value = paste0(current_dat_box$`Humidity (%)`, "%"),
      title = "Humidity",
      icon = icon("tint"), 
      color = case_when(current_dat_box$`Humidity (%)` >= 60 & current_dat_box$`Humidity (%)` < 70 ~ "teal",
                        current_dat_box$`Humidity (%)` >= 30 & current_dat_box$`Humidity (%)` < 60 ~ "olive",
                        current_dat_box$`Humidity (%)` >= 25 & current_dat_box$`Humidity (%)` < 30 ~ "teal",
                        current_dat_box$`Humidity (%)`< 25 ~ "orange"),
      subtitle = case_when(current_dat_box$`Humidity (%)` >= 60 & current_dat_box$`Humidity (%)` < 70 ~ "Fair",
                           current_dat_box$`Humidity (%)` >= 30 & current_dat_box$`Humidity (%)` < 60 ~ "Good",
                           current_dat_box$`Humidity (%)` >= 25 & current_dat_box$`Humidity (%)` < 30 ~ "Fair",
                           current_dat_box$`Humidity (%)`< 25 ~ "Low")
    )
  })
  
  output$pressure_current <- renderInfoBox({
    current_dat_box <- current_dat()
    infoBox(
      value = paste0(current_dat_box$`Pressure (mbar)`, " hPa"),
      title = "Pressure",
      icon = icon("tachometer"), 
      color = "blue"
    )
  })
  output$radon_current <- renderInfoBox({
    current_dat_box <- current_dat()
    infoBox(
      value = paste0(current_dat_box$`Radon (pCi/L)`, " pCi/L"),
      title = "Radon (short term avg)",
      icon = fontawesome::fa_i("radiation"), 
      color = case_when(current_dat_box$`Radon (pCi/L)` <= 2.6 ~ "olive",
                        current_dat_box$`Radon (pCi/L)` < 4 ~ "teal",
                        current_dat_box$`Radon (pCi/L)` >= 4 ~ "orange"),
      subtitle = case_when(current_dat_box$`Radon (pCi/L)` <= 2.6 ~ "Good",
                        current_dat_box$`Radon (pCi/L)` < 4 ~ "Fair",
                        current_dat_box$`Radon (pCi/L)` >= 4 ~ "Poor")
    )
    
  })
  output$co2_current <- renderInfoBox({
    current_dat_box <- current_dat()
    infoBox(
      value = paste0(current_dat_box$`CO2 (ppm)`, " ppm"),
      title = "CO2",
      icon = icon("cloud"), 
      color = case_when(current_dat_box$`CO2 (ppm)` < 400 ~ "olive",
                        current_dat_box$`CO2 (ppm)` >= 400 & current_dat_box$`CO2 (ppm)` < 1000 ~ "teal",
                        current_dat_box$`CO2 (ppm)` >= 1000 & current_dat_box$`CO2 (ppm)` < 2000 ~ "yellow",
                        current_dat_box$`CO2 (ppm)` >= 2000 & current_dat_box$`CO2 (ppm)` < 5000 ~ "orange",
                        current_dat_box$`CO2 (ppm)` >= 5000  ~ "red"),
      subtitle = case_when(current_dat_box$`CO2 (ppm)` < 400 ~ "Background",
                        current_dat_box$`CO2 (ppm)` >= 400 & current_dat_box$`CO2 (ppm)` < 1000 ~ "Fair",
                        current_dat_box$`CO2 (ppm)` >= 1000 & current_dat_box$`CO2 (ppm)` < 2000 ~ "Poor",
                        current_dat_box$`CO2 (ppm)` >= 2000 & current_dat_box$`CO2 (ppm)` < 5000 ~ "High",
                        current_dat_box$`CO2 (ppm)` >= 5000  ~ "Extreme")

    )
  })
  output$voc_current <- renderInfoBox({
    current_dat_box <- current_dat()
    infoBox(
      value = paste0(current_dat_box$`VOC (ppb)`, " ppb"),
      title = "VOCs",
      icon = fontawesome::fa_i("wind"), 
      color = case_when(current_dat_box$`VOC (ppb)`< 250 ~ "olive",
                        current_dat_box$`VOC (ppb)` >= 250 & current_dat_box$`VOC (ppb)` < 2000 ~ "teal",
                        current_dat_box$`VOC (ppb)` >= 2000 ~ "orange"),
      subtitle = case_when(current_dat_box$`VOC (ppb)`< 250 ~ "Low",
                        current_dat_box$`VOC (ppb)` >= 250 & current_dat_box$`VOC (ppb)` < 2000 ~ "Med",
                        current_dat_box$`VOC (ppb)` >= 2000 ~ "High")
    )
  })
  output$pm2.5_current <- renderInfoBox({
    current_dat_box <- current_dat()
    infoBox(
      value = paste0(current_dat_box$`PM2.5 (ug/m3)`, " ug/m3"),
      title = "PM 2.5",
      icon = fontawesome::fa_i("smog"), 
      color = case_when(current_dat_box$`PM2.5 (ug/m3)` < 10 ~ "olive",
                        current_dat_box$`PM2.5 (ug/m3)` >= 10 & current_dat_box$`PM2.5 (ug/m3)` < 25 ~ "teal",
                        current_dat_box$`PM2.5 (ug/m3)` >=25 ~ "orange"),
      subtitle = case_when(current_dat_box$`PM2.5 (ug/m3)` < 10 ~ "Good",
                        current_dat_box$`PM2.5 (ug/m3)` >= 10 & current_dat_box$`PM2.5 (ug/m3)` < 25 ~ "Fair",
                        current_dat_box$`PM2.5 (ug/m3)` >=25 ~ "Poor")
    )
  })
  output$pm10_current <- renderInfoBox({
    current_dat_box <- current_dat()
    infoBox(
      value = paste0(current_dat_box$`PM10 (ug/m3)`, " ug/m3"),
      title = "PM 10",
      icon = icon("smog"), 
      color = case_when(current_dat_box$`PM2.5 (ug/m3)` < 10 ~ "olive",
                        current_dat_box$`PM2.5 (ug/m3)` >= 10 & current_dat_box$`PM2.5 (ug/m3)` < 25 ~ "teal",
                        current_dat_box$`PM2.5 (ug/m3)` >=25 ~ "orange"),
      subtitle = case_when(current_dat_box$`PM2.5 (ug/m3)` < 10 ~ "Good",
                           current_dat_box$`PM2.5 (ug/m3)` >= 10 & current_dat_box$`PM2.5 (ug/m3)` < 25 ~ "Fair",
                           current_dat_box$`PM2.5 (ug/m3)` >=25 ~ "Poor")
    )
  })
  
  
  
  
  output$airplot <- renderPlotly({
    req(input$metric)
    if(is.null(input$roavg)){

      show_dat <- dat() %>% 
        filter(metric %in% c(input$metric)) 
      
      air_plot <- ggplot(show_dat, aes(x = recorded, y = Result ,
                                       label = Time,
                                       label2 = date)) +
        geom_line(aes(color = date), size = 1) +
        viridis::scale_color_viridis(direction = -1) +
        facet_wrap(~metric, ncol = 1, scales = "free_y") +
        ggthemes::theme_pander() +
        xlab("Date")+
        ylab("Concentration") +
        theme(legend.position = "none",
              panel.grid.major.y = element_blank(),
              panel.grid.major.x = element_line(color = "snow2"),
              strip.text.x = element_text(color = "#556B2F", face = "bold"),
              text = element_text(family = "Arial"))
      
    }
    else{
      
      show_dat <- dat() %>% 
        filter(metric %in% c(input$metric)) %>% 
        pivot_longer(names_to = "roavg_period", values_to = "mean", cols = contains("mean")) %>% 
        filter(roavg_period %in% c(input$roavg))
      
      air_plot <- ggplot(show_dat, aes(x = recorded, y = Result ,
                                       label = Time,
                                       label2 = date)) +
        geom_line(size = 1, alpha = 0.5) +
        viridis::scale_color_viridis(discrete = TRUE, begin = 0, end = 0.75) +
        geom_line(mapping = aes(x = recorded, y = mean, color = roavg_period),
                  size = 1, linetype = "dashed") +
        facet_wrap(~metric, ncol = 1, scales = "free_y") +
        ggthemes::theme_pander() +
        xlab("Date")+
        ylab("Concentration") +
        theme( panel.grid.major.y = element_blank(),
              panel.grid.major.x = element_line(color = "snow2"),
              strip.text.x = element_text(color = "#556B2F", face = "bold"),
              text = element_text(family = "Arial"))
      
    }
    
    
    
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
    
    # sub_plot <- ggplot(sub_dat) +
    #     geom_line(aes(x = recorded, y = Result ,
    #                   color = metric,
    #                   alpha = metric,
    #                   label = Time,
    #                   label2 = date)) +
    #     scale_color_manual(values = c("#cf9c2e", "#ec7f78"))+
    #     scale_alpha_manual(values = c(0.75, .5))+
    #     #viridis::scale_color_viridis() +
    #     #facet_wrap(~metric, ncol = 1, scales = "free_y") +
    #     ggthemes::theme_pander() +
    #     xlab("Date")+
    #     ylab("Concentration") +
    #     theme(legend.position = "none",
    #           panel.grid.major.y = element_blank(),
    #           panel.grid.major.x = element_line(color = "snow2"),
    #           strip.text.x = element_text(color = "#556B2F", face = "bold"),
    #           text = element_text(family = "Arial"))
    # 
    # sub_plotly <- ggplotly(sub_plot,
    #                        tooltip = c("y", "label", "label2"))
    
    
    sep_pm_plot <- ggplot(sub_dat) +
      geom_line(aes(x = recorded, y = Result ,
                    color = metric,
                    label = Time,
                    label2 = date)) +
      # geom_line(mapping = aes(x = recorded, y = mean7day), 
      #           size = 1, color = "#ec7f78", linetype = "dashed") + 
      # geom_line(mapping = aes(x = recorded, y = mean24hr), 
      #           size = 1, color = "grey", linetype = "dashed") + 
      # geom_line(mapping = aes(x = recorded, y = mean1hr), 
      #           size = 1, color = "light blue", linetype = "dashed") + 
      scale_color_manual(values = c("#cf9c2e", "#7ab4b1"))+
      facet_wrap(~metric, ncol = 1, scales = "free_y") +
      ggthemes::theme_pander() +
      xlab("Date")+
      ylab("Concentration") +
      theme(legend.position = "none",
            panel.grid.major.y = element_blank(),
            panel.grid.major.x = element_line(color = "snow2"),
            strip.text.x = element_text(color = "#556B2F", face = "bold"),
            text = element_text(family = "Arial"), 
            panel.spacing = unit(2, "lines"))
    
    
    sep_pm_plotly <- ggplotly(sep_pm_plot, 
                              tooltip = c("y", "label", "label2"))
    
    sep_pm_box <-  ggplot(sub_dat) + 
      geom_boxplot(aes(x = "Current\nRange", y = Result, fill = metric)) + 
      geom_boxplot(air_dat_long %>%
                     filter(str_detect(metric, "PM")), 
                   mapping = aes(x = "All\nMeasurements", 
                                 y = Result, fill = metric)) +
      scale_fill_manual(values = c("#cf9c2e", "#7ab4b1")) +
      facet_wrap(~metric, ncol = 1) + 
      ggthemes::theme_pander() +
      theme(legend.position = "none",
            panel.grid.major.y = element_blank(),
            panel.grid.major.x = element_line(color = "snow2"), 
            text = element_text(family = "Arial"),
            panel.spacing = unit(2, "lines"))
    
    sep_pm_boxly <- ggplotly(sep_pm_box)
    
    
    # sub_box <- ggplot(sub_dat) +
    #     geom_boxplot(aes(x = "Current\nRange", y = Result, fill = metric)) +
    #     geom_boxplot(air_dat_long %>%
    #                      filter(str_detect(metric, "PM")),
    #                  mapping = aes(x = "All\nMeasurements",
    #                                y = Result, fill = metric)) +
    #     scale_fill_manual(values = c("#cf9c2e", "#ec7f78")) +
    #     ggthemes::theme_pander() +
    #     theme(legend.position = "none",
    #           panel.grid.major.y = element_blank(),
    #           panel.grid.major.x = element_line(color = "snow2"),
    #           text = element_text(family = "Arial"))
    # sub_box_ly <- ggplotly(sub_box)
    
    subplot(list(sep_pm_plotly, sep_pm_boxly), widths = c(.8, .2), margin = 0)
    
    
    # subplot(list(sub_plotly, sub_box_ly), widths = c(.9, .1),
    #         nrows = 1, shareY = TRUE, margin = 0))
    
    
  })
  output$pm_density <- renderPlotly({
    dens_plotly(dat(), c("PM2.5 (ug/m3)", "PM10 (ug/m3)"))
    
  })
  
  output$pm_xy <- renderPlotly({
    req(input$pm_y_comp)
    
    new_dat <- dat() %>%
      select(-recorded) %>% 
      mutate(hour_match = hour(time),
             minute_match = minute(time)) %>%
      select(metric, date, hour_match, minute_match, Result)  %>%
      pivot_wider(names_from = "metric", values_from = "Result") %>%
      select(`PM2.5 (ug/m3)`,`PM10 (ug/m3)`, input$pm_y_comp, date, hour_match, minute_match) %>%
      rename(y = input$pm_y_comp)
    
    pm <- highlight_key(new_dat)
    if(!input$pm_y_comp %in% c(c("PM2.5 (ug/m3)", "PM10 (ug/m3)")) ){
      pm2.5 <- plot_ly(pm, x = ~`PM2.5 (ug/m3)`, y = ~y) %>%
        add_markers(showlegend = FALSE) %>% 
        highlight("plotly_selected")
      pm10 <- plot_ly(pm, x = ~`PM10 (ug/m3)`, y = ~y) %>%
        add_markers(showlegend = FALSE) %>% 
        highlight("plotly_selected")
    }
    else if(input$pm_y_comp == "PM2.5 (ug/m3)"){
      pm2.5 <- plot_ly(pm, x = ~y, y = ~y) %>%
        add_markers(showlegend = FALSE) %>% 
        highlight("plotly_selected")
      pm10 <- plot_ly(pm, x = ~`PM10 (ug/m3)`, y = ~y) %>%
        add_markers(showlegend = FALSE) %>% 
        highlight("plotly_selected")
    }
    else if(input$pm_y_comp == "PM10 (ug/m3)"){
      pm2.5 <- plot_ly(pm, x = ~`PM2.5 (ug/m3)`, y = ~y) %>%
        add_markers(showlegend = FALSE) %>% 
        highlight("plotly_selected")
      pm10 <- plot_ly(pm, x = ~y, y = ~y) %>%
        add_markers(showlegend = FALSE) %>% 
        highlight("plotly_selected")
    }
    
    
    subplot(list(pm2.5, pm10), titleX = TRUE)
    
    #bscols(width = c(2,2), pm2.5, pm10)
    
  })
  
  
  output$cooking_log <- DT::renderDataTable(
    cooking_log_shiny, 
    options = list(pageLength = 10), 
    rownames= FALSE
  )
  
  # output$cooking_events_TS <- renderPlotly({
  #   cooking_events_ts_plot <- cooking_plot_dat %>% 
  #     ggplot() + 
  #     geom_line(mapping = aes(x = hms::as_hms(recorded), y = Result)) + 
  #     geom_rect(data=cooking_log, mapping=aes(xmin = hms::as_hms(start_fmt), 
  #                                             xmax= hms::as_hms(end_fmt), 
  #                                             ymin=0, ymax=Inf, fill = intervention),
  #               alpha=0.5) +
  #     scale_fill_viridis(discrete = TRUE) +
  #     #scale_x_time(labels = scales::label_time(format = '%H:%M')) +
  #     scale_x_time(labels = scales::label_time(format = '%H')) +
  #     facet_grid(rows = vars(metric), cols = vars(event_no), scales = "free")  +
  #     theme_pander() +
  #     xlab("Time")+
  #     ylab("Concentration") + 
  #     theme(legend.position = "top",
  #           panel.grid.major.y = element_blank(),
  #           panel.grid.major.x = element_line(color = "snow2"),
  #           strip.text.x = element_text(color = "#404040", face = "bold"),
  #           text = element_text(family = "Arial"))
  #   
  #   ggplotly(cooking_events_ts_plot)
  #   
  # })
  
  output$avg_change <- renderPlotly({
    avg_change_plot <- ggplot(time_passed) + 
      geom_line(aes(x = newtime, y = Result, group = event_no, color = intervention), 
                alpha = 0.4, linetype = "dashed") + 
      geom_line(avg_time_passed, mapping = aes(x = newtime, y = Result, color = intervention)) + 
      viridis::scale_color_viridis(discrete = TRUE) + 
      facet_grid(rows = vars(metric), cols = vars(intervention), scales = "free") + 
      ggthemes::theme_pander() +
      xlab("# of minutes")+
      ylab("Concentration") + 
      theme(legend.position = "none",
            panel.grid.major.y = element_blank(),
            panel.grid.major.x = element_line(color = "snow2"),
            strip.text.x = element_text(color = "#404040", face = "bold"),
            text = element_text(family = "Arial"))
    
    ggplotly(avg_change_plot)
    
  })
  
  
})
