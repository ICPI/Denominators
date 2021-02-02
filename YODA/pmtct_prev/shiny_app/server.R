library(shiny)
library(leaflet)
library(mapdeck)
library(RColorBrewer)
library(sp)
library(sf)
options(encoding = 'UTF-8')
load("data/data.RData")
df_plot_sf <- as(df_plot,"sf")
df_site_plot_sf <- sf::st_as_sf(df_site_plot, coords = c("longitude", "latitude"))
key <- "pk.eyJ1IjoiaWZlbGxvd3MiLCJhIjoiY2tmNDd3dXZrMGFqOTJzb2V2azB3YnZ5aCJ9.nG777E-EH37e5wAJdsykug"


shinyServer(function(input, output, session) {

    
    last_country <- reactiveVal("")
    plot_data <- reactiveVal(NULL)
    nchunks <- 20
    vol <- new.env()
    vol$index <- -1
    
    observe({
        plot_data()
        vol$index <- 1
        updateNumericInput(session, "index",value=1)
    })
    
    observeEvent(input$index, {
        df <- isolate(plot_data())
        if(is.null(df))
            return(NULL)
        i <- input$index
        if(i < 1 | i != vol$index){
            return(NULL)
        }
        min_send <- if(vol$plot_area) 30 else 100
        print(i)
        sq <- unique(ceiling(seq(from=0, to=nrow(plot_data()), length=nchunks)))
        sq2 <- unique(
            ceiling(
                c(seq(
                    from=0, 
                    to=nrow(df), 
                    by=max(.5, min(nrow(df)-1, min_send))
                    ),
                nrow(df)))
            )
        if(length(sq2) < length(sq))
            sq <- sq2
        if(i > length(sq)-1){
            updateNumericInput(session, "index",value=-1)
            return(NULL)
        }
        if(i == 1){
            for(j in 1:nchunks){
                mapdeck_update(map_id = "map") %>%
                    clear_scatterplot(paste0("scatter",j)) %>%
                    clear_geojson(paste0("poly",j))
            }
        }
        df <- df[(sq[i]+1):sq[i+1],]
        if(i == 1)
            js <- mapdeck_legend(vol$leg)
        else
            js <- NULL
        if(vol$plot_area){
            mapdeck_update(map_id = "map") %>%
                add_geojson(
                    data = df,
                    tooltip = vol$popup_variable,
                    fill_colour = "fill_color",
                    legend=js,
                    update_view=FALSE,
                    auto_highlight = TRUE,
                    layer_id=paste0("poly",i)
                )
        }else{
            
            mapdeck_update(map_id = "map") %>%
                add_scatterplot(
                    data = df,
                    lat = "latitude",
                    lon = "longitude",
                    fill_colour = "fill_color",
                    #stroke_width=4,
                    #stroke_colour = "fill_color",
                    tooltip = vol$popup_variable,
                    radius = 1500,
                    radius_min_pixels = 3,
                    legend=js,
                    #palette = "reds",
                    #legend=js,
                    update_view=FALSE,
                    #auto_highlight = FALSE,
                    layer_id=paste0("scatter",i)
                )
        }

        vol$index <- i + 1
        updateNumericInput(session, "index",value=i+1)
    })
    
    
    
    
    active_data <- reactive({
        if(input$plot_type == "Areas"){
            df <- df_plot_sf
        }else{
            df <- df_site_plot_sf
            df$yield <- df$hts_tst_pos / df$hts_tst_tot
        }
        if(input$country == "All"){
            df
        }else{
            df[df$countryname == input$country,]
        }
    })
    
    
    # draw empty map
    output$map <- renderMapdeck({
        mapdeck(
            token = key,
            #pitch = 35,
            style = 'mapbox://styles/mapbox/dark-v10'
        )
    })
    
    observe({
        legend_label <- "Smoothed Adjusted Yield"
        #dfp <- active_data()
        dfp <- active_data()
        df_plot_sub <- dfp[dfp$time == as.numeric(input$time),]
        
        if(input$metric == "Raw"){
            df_plot_sub$fitted <- df_plot_sub$yield
            df_plot$fitted <- df_plot$yield
            df_plot_sub <- df_plot_sub[!is.na(df_plot_sub$yield),]
            df_plot <- df_plot[!is.na(df_plot$yield),]
            legend_label <- "Yield"
        }
        
        df_plot_sub <- df_plot_sub[!is.na(df_plot_sub$fitted),]
        if(nrow(df_plot_sub) == 0){
            mapdeck_update(map_id = "map") %>%
                clear_geojson() %>%
                clear_scatterplot()
            return(NULL)
        }
        if(input$scale_to_time)
            values <- df_plot_sub$fitted
        else
            values <- df_plot$fitted
        pal <- leaflet::colorNumeric("Reds", domain=c(0,values))
        
        df_plot_sub$fill_color <- pal(df_plot_sub$fitted)
        q <- seq(from=0, to=max(df_plot_sub$fitted),length=4)
        #c(0, quantile(df_plot_sub$fitted,c(.25, .5, .75,1)))
        if(input$country == "All")
            popup_variable <- NULL
        else
            popup_variable <- "popup_html"
        l1 <- legend_element(
            variables = paste0(round(q*100),"%"),
            colours = pal(q),
            colour_type = "fill",
            variable_type = "gradient",
            title = legend_label
        )
        vol$leg <- l1
        vol$popup_variable <- popup_variable
        #js <- mapdeck_legend(l1)
        if(input$plot_type == "Areas"){
            vol$plot_area <- TRUE
            plot_data(df_plot_sub)
            # mapdeck_update(map_id = "map") %>%
            #     clear_scatterplot("scatter") %>%
            #     clear_geojson("poly") %>%
            #     add_geojson(
            #         data = df_plot_sub,
            #         tooltip = popup_variable,
            #         fill_colour = "fill_color",
            #         legend=js,
            #         update_view=FALSE,
            #         auto_highlight = TRUE,
            #         layer_id="poly"
            #     )
        }else{
            vol$plot_area <- FALSE
            df_plot_sub[[legend_label]] <- df_plot_sub$fitted*100
            df_plot_sub$fill_color <- paste0(df_plot_sub$fill_color,"FF")
            plot_data(df_plot_sub)
            # mapdeck_update(map_id = "map") %>%
            #     clear_scatterplot("scatter") %>%
            #     clear_geojson("poly") %>%
            #     add_scatterplot(
            #         data = df_plot_sub,
            #         lat = "latitude",
            #         lon = "longitude",
            #         fill_colour = legend_label,
            #         #stroke_width=4,
            #         #stroke_colour = "fill_color",
            #         tooltip = popup_variable,
            #         radius = 1500,
            #         radius_min_pixels = 3,
            #         legend=TRUE,
            #         palette = "reds",
            #         #legend=js,
            #         update_view=FALSE,
            #         #auto_highlight = FALSE,
            #         layer_id="scatter"
            #     )
        }
    })
    
    
    # scale map bounds on country change
    observe({
        lc <-  isolate(last_country())
        if(input$country != lc){
            globe <- input$country == "All"
            df <- df_site_plot[!is.na(df_site_plot$latitude) &
                                   !is.na(df_site_plot$longitude),]
            if(!globe){
                df <- df[df$countryname == input$country,]
            }
            if(nrow(df) == 0)
                return(NULL)
            isolate(last_country(input$country))
            mapdeck_update(map_id = "map") %>%
                mapdeck_view(
                    c(
                        median(df$longitude), 
                        median(df$latitude)
                    ),
                    zoom=if(globe) 2 else 4,
                    duration=1000,
                    transition = "fly"
                )
            #leafletProxy("map") %>%
            #    flyToBounds(
            #        min(df$longitude),
            #        min(df$latitude),
            #        max(df$longitude),
            #        max(df$latitude)
            #    )
        }
    })

    # observe({
    #     if(input$country != isolate(last_country()) || input$country == "All"){
    #         df <- df_site_plot[!is.na(df_site_plot$latitude) & 
    #                                !is.na(df_site_plot$longitude),]
    #         if(input$country != "All"){
    #             df <- df[df$countryname == input$country,]
    #         }
    #         if(nrow(df) == 0)
    #             return(NULL)
    #         isolate(last_country(input$country))            
    #         m <- draw_map()
    #         leaflet_map(m)
    #     }
    # })
    # draw shapes/points
    #observe({
    #    if(input$country == isolate(last_country()) && input$country != "All"){
    #        draw_map(TRUE)
    #    }
    #})

})
