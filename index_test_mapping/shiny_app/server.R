library(tidyverse)
library(shiny)
library(leaflet)
library(mapdeck)
library(RColorBrewer)
library(sp)
library(sf)
library(Hmisc)
library(stringr)
options(encoding = 'UTF-8')

key <- "pk.eyJ1IjoiaWZlbGxvd3MiLCJhIjoiY2tmNDd3dXZrMGFqOTJzb2V2azB3YnZ5aCJ9.nG777E-EH37e5wAJdsykug"

load("data/data.RData")

#df_site_plot <- df_site_plot[order(st_coordinates(df_site_plot)[,2]),]

shinyServer(function(input, output, session) {
  
  last_country <- reactiveVal("")
  
  plot_data <- reactiveVal(NULL)

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
   print(i)
   sq <- unique(ceiling(seq(from=0, to=nrow(plot_data()), length=100)))
   if(i > length(sq)-1){
     updateNumericInput(session, "index",value=-1)
     return(NULL)
   }
   if(i == 1){
     for(j in 1:100){
       mapdeck_update(map_id = "map") %>%
         clear_scatterplot(paste0("scatter",j))
     }
   }
   df <- plot_data()[(sq[i]+1):sq[i+1],]
   mapdeck_update(map_id = "map") %>%
     add_scatterplot(
       data = df,
       lat = "latitude",
       lon = "longitude",
       fill_colour = "fill_color",#df$legend_label[1],
       #stroke_width=4,
       #stroke_colour = "fill_color",
       #tooltip = "pop",
       tooltip = "pop",
       radius = 1500,
       radius_min_pixels = 3,
       #palette = NULL,
       legend=if(i==1) mapdeck_legend(vol$leg) else FALSE,
       update_view=FALSE,
       #auto_highlight = FALSE,
       layer_id=paste0("scatter",i)
     )
   vol$index <- i + 1
   updateNumericInput(session, "index",value=i+1)
  })
  
  # draw empty map
  output$map <- renderMapdeck({
    mapdeck(
      token = key,
      style = 'mapbox://styles/mapbox/dark-v10'
    ) #%>% 
      #htmlwidgets::onRender( #it would be great to add a navigation widget
      #  "function(el, x){console.log('.');nav = new mapboxgl.NavigationControl();mapmap._map.getMap().addControl(nav, 'top-left');}"
      #)
  })
  
  
  
  observe({
    if(input$country != "All"){
      psnus <- unique(df_site_plot$psnu[df_site_plot$countryname == input$country])
      updateSelectInput(
        session, 
        "psnu", 
        choices = c("All", sort(psnus)), 
        selected = "All")
    }
  })
  
  get_data <- function(){
    df_plot_sub <- df_site_plot
    if(input$country != "All"){
      df_plot_sub <- df_plot_sub[df_plot_sub$countryname == input$country, ]
      if(input$psnu != "All")
        df_plot_sub <- df_plot_sub[df_plot_sub$psnu == input$psnu, ]
    }
    df_plot_sub <- df_plot_sub[
      df_plot_sub$time == as.numeric(input$time) & 
        df_plot_sub$hts_tst_pos_non_index > input$non_index_pos &
        df_plot_sub$facility != "N/A", ]
    if(input$metric == "Smoothed"){
      if(input$plot_type == "# Index Pos. / # Non-Index Pos."){
        metric <- df_plot_sub$fitted_pos_per_non_pos
      }else if(input$plot_type == "# Index Tsts. / # Non-Index Pos."){
        metric <- df_plot_sub$fitted_tsts_per_non_pos
      }else{
        metric <- df_plot_sub$hts_tst_pos_index / df_plot_sub$hts_tst_tot_index
      }
    }else{
      if(input$plot_type == "# Index Pos. / # Non-Index Pos."){
        metric <- df_plot_sub$hts_tst_pos_index / df_plot_sub$hts_tst_pos_non_index
      }else if(input$plot_type == "# Index Tsts. / # Non-Index Pos."){
        metric <- df_plot_sub$hts_tst_tot_index / df_plot_sub$hts_tst_pos_non_index
      }else{
        metric <- df_plot_sub$hts_tst_pos_index / df_plot_sub$hts_tst_tot_index
      }      
    }
    df_plot_sub$metric <- metric
    df_plot_sub
  }
  
  observe({
    if(input$metric == "Smoothed" && input$plot_type != "Index Yield"){
      legend_label <- paste("Smoothed", input$plot_type)
    }else{
      legend_label <- input$plot_type
    }
    df_plot_sub <- get_data()
    if(nrow(df_plot_sub[!is.na(df_plot_sub$metric),]) == 0){
      for(i in 1:100)
        mapdeck_update(map_id = "map") %>%
          clear_scatterplot(paste0("scatter",i))
      return(NULL)
    }
    df_plot_sub[[legend_label]] <-Hmisc::cut2(round(df_plot_sub$metric, 2), g=6)
    
    if(input$country != "All"){
      df_plot_sub$pop <- df_plot_sub$popup_html
    }else{
      df_plot_sub$pop <- paste0(df_plot_sub$facility,"</br></br> (select a country for detail)")
    }
    
    df_plot_sub <- df_plot_sub[!is.na(df_plot_sub[[legend_label]]), ]
    vol$pal <- leaflet::colorNumeric("Reds", domain=as.numeric(df_plot_sub[[legend_label]]))
    df_plot_sub$fill_color <- paste0(
      vol$pal(as.numeric(df_plot_sub[[legend_label]])),
      "FF"
    )
    levs <- levels(df_plot_sub[[legend_label]])
    
    vol$leg <- legend_element(
      variables = levs,
      colours = vol$pal(1:length(levs)),
      colour_type = "fill",
      #variable_type = "gradient",
      title = legend_label
    )
    #df_plot_sub$legend_label <- legend_label
    plot_data(df_plot_sub)
  })
  
  # scale map bounds on country change
  observe({
    lc <-  isolate(last_country())
    if(input$country != lc){
      globe <- input$country == "All"
      df <- st_coordinates(df_site_plot)
      if(!globe){
        df <- df[df_site_plot$countryname == input$country,]
      }
      if(nrow(df) == 0)
        return(NULL)
      isolate(last_country(input$country))
      mapdeck_update(map_id = "map") %>%
        mapdeck_view(
          c(
            median(df[,1]), 
            median(df[,2])
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
  
  
  table_data <- reactive({
    df_plot_sub <- get_data()
    df <- df_plot_sub %>% 
      as.tibble() %>% 
      mutate(
        non_index = hts_tst_tot_non_index,
        pos_non_index = hts_tst_pos_non_index,
        index=hts_tst_tot_index,
        pos_index=hts_tst_pos_index,
        pos_per_non_index_pos = round(hts_tst_pos_index / hts_tst_pos_non_index, 2),
        tsts_per_non_index_pos = round(hts_tst_tot_index / hts_tst_pos_non_index, 2),
        index_yield = round(hts_tst_pos_index / hts_tst_tot_index, 2),
        smooth_tsts_per_non_pos = round(fitted_tsts_per_non_pos, 2),
        smooth_pos_per_non_pos = round(fitted_pos_per_non_pos, 2)
      ) %>%
      arrange(countryname, psnu, facilityuid, time) %>%
      select(countryname, psnu, facility, 
             fiscal_year, quarter, 
             non_index, pos_non_index, 
             index, pos_index, 
             pos_per_non_index_pos, tsts_per_non_index_pos,
             index_yield,
             smooth_tsts_per_non_pos, smooth_pos_per_non_pos)
    df
  })
  
  output$table <- DT::renderDataTable({
      df <- table_data()
      names(df) <- str_replace_all(names(df), "_"," ")
      df
    })
  
  output$yield_scatter <- renderPlotly({
    df <- table_data()
    (
      ggplot(
        data=df,
        aes(
          y=index_yield, 
          x=tsts_per_non_index_pos, 
          text = facility
        )) +
        geom_point() + 
        scale_x_log10() +
        ylab("Index Yield") +
        xlab("# Index Tests / # Non-Index Pos.") +
        theme_bw()
    ) %>%
    plotly::ggplotly(tooltip = "text") %>%
    print()
  })
  
  output$yield_scatter2 <- renderPlotly({
    df <- table_data()
    (
      ggplot(
        data=df,
        aes(
          y=index_yield, 
          x=non_index, 
          text = facility
        )) +
        geom_point() + 
        scale_x_log10() +
        ylab("Index Yield") +
        xlab("# Non-Index Pos.") +
        theme_bw()
    ) %>%
      plotly::ggplotly(tooltip = "text")  %>%
      print()
  })
})