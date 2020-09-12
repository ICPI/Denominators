library(shiny)
library(leaflet)
library(RColorBrewer)
library(sp)

load("data/data.RData")

shinyServer(function(input, output, session) {
    # # Reactive expression for the data subsetted to what the user selected
    # filteredData <- reactive({
    #     quakes[quakes$mag >= input$range[1] & quakes$mag <= input$range[2],]
    # })
    # 
    # # This reactive expression represents the palette function,
    # # which changes as the user makes selections in UI.
    # colorpal <- reactive({
    #     colorNumeric(input$colors, quakes$mag)
    # })
    
    last_country <- reactiveVal("")
    
    active_data <- reactive({
        if(input$plot_type == "Areas"){
            df <- df_plot
        }else{
            df <- df_site_plot
            df$yield <- df$hts_tst_pos / df$hts_tst_tot
        }
        if(input$country == "All"){
            df
        }else{
            df[df$countryname == input$country,]
        }
    })
    output$map <- renderLeaflet({
        leaflet() %>%
            addProviderTiles(providers$CartoDB.Positron) 
    })
    observe({
        if(input$country != last_country()){
            df <- df_site_plot[!is.na(df_site_plot$latitude) & 
                                   !is.na(df_site_plot$longitude),]
            if(input$country != "All"){
                df <- df[df$countryname == input$country,]
            }
            last_country(input$country)
            leafletProxy("map") %>%
                flyToBounds(
                    min(df$longitude),
                    min(df$latitude),
                    max(df$longitude),
                    max(df$latitude)
                )
        }
    })
    observe({
        
        dfp <- active_data()
        df_plot_sub <- dfp[dfp$time == input$time,]
        if(input$metric == "Raw"){
            df_plot_sub$fitted <- df_plot_sub$yield
        }
        df_plot_sub <- df_plot_sub[!is.na(df_plot_sub$fitted),]
        if(input$scale_to_time)
            color_domain <- c(0, df_plot_sub$fitted)
        else
            color_domain <- c(0, dfp$fitted)
        pal <- colorNumeric("Reds", domain=color_domain)
        if(input$plot_type == "Areas"){
            leafletProxy("map", data = df_plot_sub) %>%
                clearShapes() %>%
                clearControls() %>%
            addPolygons(
                fillColor=~pal(fitted),#pal(fitted),
                weight=.25,
                color="#ffffff",
                fillOpacity = 1,
                popup = df_plot_sub$popup_html,
                popupOptions = popupOptions(maxWidth = 1000)
            ) %>%
                addLegend("bottomright", pal = pal, values = color_domain,
                          title = "Smoothed Adjusted Yield",
                          bins=4,
                          opacity = 1,
                          labFormat = labelFormat(
                              suffix = "%", 
                              transform=function(x) x*100
                          )
                )
        }else{
            leafletProxy("map", data = df_plot_sub) %>%
                clearShapes() %>%
                clearControls() %>%
                addCircles(
                    lng=~longitude, 
                    lat=~latitude,
                    color=~pal(fitted),
                    popup = df_plot_sub$popup_html,
                    popupOptions = popupOptions(maxWidth = 1000)
                ) %>%
                addLegend("bottomright", pal = pal, values = color_domain,
                          title = "Smoothed Adjusted Yield",
                          bins=4,
                          opacity = 1,
                          labFormat = labelFormat(
                              suffix = "%", 
                              transform=function(x) x*100)
                )
        }
    })
    # output$map <- renderLeaflet({
    #     m <- NULL
    #     dfp <- active_data()
    #     df_plot_sub <- dfp[dfp$time == input$time,]
    #     if(input$metric == "Raw"){
    #         df_plot_sub$fitted <- df_plot_sub$yield
    #     }
    #     df_plot_sub <- df_plot_sub[!is.na(df_plot_sub$fitted),]
    #     if(input$scale_to_time)
    #         color_domain <- c(0, df_plot_sub$fitted)
    #     else
    #         color_domain <- c(0, dfp$fitted)
    #     pal <- colorNumeric("Reds", domain=color_domain)
    #     if(input$plot_type == "Areas"){
    #         m <- df_plot_sub %>%
    #             leaflet() %>%
    #             addProviderTiles(providers$CartoDB.Positron) %>%
    #             addPolygons(
    #                 fillColor=~pal(fitted),#pal(fitted),
    #                 weight=.25,
    #                 color="#ffffff",
    #                 fillOpacity = 1,
    #                 popup = df_plot_sub$popup_html,
    #                 popupOptions = popupOptions(maxWidth = 1000)
    #             ) %>%
    #             addLegend("bottomright", pal = pal, values = color_domain,
    #                       title = "Smoothed Adjusted Yield",
    #                       bins=4,
    #                       opacity = 1,
    #                       labFormat = labelFormat(
    #                           suffix = "%", 
    #                           transform=function(x) x*100
    #                           )
    #             )
    #     }else if(input$plot_type == "Facilities"){
    #         m <- df_plot_sub %>%
    #             leaflet() %>%
    #             addProviderTiles(providers$CartoDB.Positron) %>%
    #             addCircles(
    #                 lng=~longitude, 
    #                 lat=~latitude,
    #                 color=~pal(fitted),
    #                 popup = df_plot_sub$popup_html,
    #                 popupOptions = popupOptions(maxWidth = 1000)
    #             ) %>%
    #             addLegend("bottomright", pal = pal, values = color_domain,
    #                       title = "Smoothed Adjusted Yield",
    #                       bins=4,
    #                       opacity = 1,
    #                       labFormat = labelFormat(
    #                           suffix = "%", 
    #                           transform=function(x) x*100)
    #             )
    #     }
    #     m
    #     # Use leaflet() here, and only include aspects of the map that
    #     # won't need to change dynamically (at least, not unless the
    #     # entire map is being torn down and recreated).
    #     #leaflet(quakes) %>% addTiles() %>%
    #     #    fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat))
    # })
    
    # # Incremental changes to the map (in this case, replacing the
    # # circles when a new color is chosen) should be performed in
    # # an observer. Each independent set of things that can change
    # # should be managed in its own observer.
    # observe({
    #     pal <- colorpal()
    #     
    #     leafletProxy("map", data = filteredData()) %>%
    #         clearShapes() %>%
    #         addCircles(radius = ~10^mag/10, weight = 1, color = "#777777",
    #                    fillColor = ~pal(mag), fillOpacity = 0.7, popup = ~paste(mag)
    #         )
    # })
    # 
    # # Use a separate observer to recreate the legend as needed.
    # observe({
    #     proxy <- leafletProxy("map", data = quakes)
    #     
    #     # Remove any existing legend, and only if the legend is
    #     # enabled, create a new one.
    #     proxy %>% clearControls()
    #     if (input$legend) {
    #         pal <- colorpal()
    #         proxy %>% addLegend(position = "bottomright",
    #                             pal = pal, values = ~mag
    #         )
    #     }
    # })

})
