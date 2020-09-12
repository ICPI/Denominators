library(shiny)
library(leaflet)
library(RColorBrewer)
library(sp)

load("data/data.RData")
shinyUI(bootstrapPage(
    tags$style(
        type = "text/css", 
        "
        html, body {width:100%;height:100%}
        th {
          padding: 5px;
          text-align: left;
          border-bottom: 1px solid #ddd;
          border-right: 1px solid #ddd;
        }
        td, th {
          padding: 5px;
          text-align: left;
          border-right: 1px solid #ddd;
        }
        "),
    leafletOutput("map", width = "100%", height = "100%"),
    absolutePanel(top = 10, right = 10, class="well",style = "background-color: rgba(220,220,220,0.75);",
                  sliderInput("time", "Year", min(df_plot$time), max(df_plot$time),
                              value = max(df_plot$time), step = 0.25, ticks=FALSE, 
                              round=TRUE, sep=""
                  ),
                  checkboxInput("scale_to_time", "Scale color to current quarter", TRUE),
                  selectInput("country", "Country",
                              c("All", sort(unique(df_plot$countryname)))
                  ),
                  radioButtons("plot_type", "Show", c("Areas","Facilities")),
                  radioButtons("metric", "Metric", c("Smoothed","Raw"))
    )
))