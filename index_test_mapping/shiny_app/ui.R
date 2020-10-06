library(shiny)
library(leaflet)
library(RColorBrewer)
library(sp)
library(mapdeck)
library(plotly)

load("data/data.RData")

shinyUI(fillPage(
  fillRow( flex = c(1,4),
  wellPanel(
    #selectInput(
    #  "time",
    #  "Year",
    #  rev(seq(from=min(df_site_plot$time), to=max(df_site_plot$time), by=.25))
    #)
    sliderInput("time", "Year", min(df_site_plot$time), max(df_site_plot$time),
                value = max(df_site_plot$time), step = 0.25, ticks=FALSE, 
                round=TRUE, sep=""
    )
    ,
    selectInput("country", "Country",
                c("All", sort(unique(df_site_plot$countryname)))
    ),
    selectInput("psnu", "PSNU",
                c("All")
    ),
    radioButtons("plot_type", "Show", c(
      "# Index Pos. / # Non-Index Pos.",
      "# Index Tsts. / # Non-Index Pos.",
      "Index Yield"
      )),
    conditionalPanel("input.plot_type != 'Index Yield'",
      radioButtons("metric", "Metric", c("Smoothed","Raw"))
    ),
    numericInput(
      "non_index_pos",
      "# Non-Index Pos. >",
      value=0,
      min=0
    ),
    conditionalPanel("false", numericInput(
      "index",
      "index",
      value=-1
    )),
    tags$style(
      type = "text/css",
      "
          th {
            padding: 5px;
            text-align: left;
            border-bottom: 1px solid #ddd;
            border-right: 1px solid #ddd;
          }
          td, th {
            padding: 5px;
            padding-bottom: 0px;
            text-align: left;
            border-right: 1px solid #ddd;
          }

          .mapdecktooltip {
          max-width: 500px;
          font-size: 10px;
          }
          "),
    tags$style(type = "text/css", "#map {height: calc(100vh - 40px) !important;}"),
    style="float: left; width:100%; height: 100%;"
  ),
  fillRow(tabsetPanel(
    tabPanel("Map",
      mapdeckOutput("map", width = "100%", height = NULL)
    ),
    tabPanel("Data",
      tags$style(type = "text/css", "dataTable {width: calc(30vw - 40px) !important;}"),
      wellPanel(
      DT::DTOutput("table"),
      style = "overflow-x:scroll; max-width: 80vw;height: calc(100vh - 40px);background-color:white;"
      )
    ),
    tabPanel("Plot",
      plotlyOutput("yield_scatter"),
      plotlyOutput("yield_scatter2")
    )
  ))
  #,
  # mapdeckOutput("map", width = "100%", height = "100%"),
  # absolutePanel(top = 10, right = 10, class="well",
  #               style = "background-color: rgba(220,220,220,0.75);min-width: 250px;",
  #               #sliderInput("time", "Year", min(df_plot$time), max(df_plot$time),
  #               #            value = max(df_plot$time), step = 0.25, ticks=FALSE, 
  #               #            round=TRUE, sep=""
  #               #),
  #               selectInput(
  #                 "time",
  #                 "Year",
  #                 rev(seq(from=min(df_plot$time), to=max(df_plot$time), by=.25))
  #               ),
  #               conditionalPanel(
  #                 "input.plot_type == 'Areas'",
  #                 checkboxInput("scale_to_time", "Scale color to current quarter", TRUE)
  #               ),
  #               selectInput("country", "Country",
  #                           c("All", sort(unique(df_plot$countryname)))
  #               ),
  #               radioButtons("plot_type", "Show", c("Areas","Facilities")),
  #               radioButtons("metric", "Metric", c("Smoothed","Raw"))
  # )
)))
