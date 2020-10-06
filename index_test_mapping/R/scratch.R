library(mapdeck)
library(sf)
library(Hmisc)
key <- "pk.eyJ1IjoiaWZlbGxvd3MiLCJhIjoiY2tmNDd3dXZrMGFqOTJzb2V2azB3YnZ5aCJ9.nG777E-EH37e5wAJdsykug"
load("shiny_app/data/data.RData")
df_plot_sub <- df_raw[df_raw$time_ind==
                        max(df_raw$time_ind)#14
                      ,]

df_plot_sub <- as(df_plot_sub,"sf")
#df_plot_sub <- df_plot_sub 
mapdeck(
  token = key,
  #pitch = 35,
  style = 'mapbox://styles/mapbox/light-v10'
) %>%
  add_geojson(
    data = df_plot_sub,
    #tooltip = "popup_html",
    fill_colour = "index_tsts_per_non_index_pos",
    legend=TRUE,
    #update_view=FALSE,
    #auto_highlight = TRUE,
    palette="reds",
    layer_id="poly"
  )



#df_site_plot <- sf::st_as_sf(df_site_plot, coords = c("longitude", "latitude"))
df_plot_sub <- df_site_plot[df_site_plot$time_ind==
                              max(df_site_plot$time_ind) &
                            !is.na(df_site_plot$fitted_tsts_per_non_pos)
                            ,]
df_plot_sub$fills <-pmin(df_plot_sub$fitted_tsts_per_non_pos, 5)
df_plot_sub$fills <-pmin(df_plot_sub$fitted_pos_per_non_pos, 2)
df_plot_sub$fills <-Hmisc::cut2(round(df_plot_sub$fitted_pos_per_non_pos,2), g=6)
df_plot_sub$fills <-Hmisc::cut2(round(df_plot_sub$fitted_tsts_per_non_pos,2), g=6)

mapdeck(
  token = key,
  #pitch = 35,
  style = 'mapbox://styles/mapbox/dark-v10'
) %>%
  add_scatterplot(
    data = df_plot_sub,
    lat = "latitude",
    lon = "longitude",
    fill_colour = "fills",
    #stroke_width=4,
    #stroke_colour = "fill_color",
    tooltip = "popup_html",
    radius = 1500,
    radius_min_pixels = 3,
    legend=TRUE,
    palette = "reds",
    #legend=js,
    #update_view=FALSE,
    #auto_highlight = FALSE,
    layer_id="scatter"
  )




(
  ggplot(
    data=df,
    aes(
      y=index_yield, 
      x=tsts_per_non_index_pos, 
      text = facility
    )) +
      geom_point() + 
    scale_x_log10() #+
#    scale_y_log10()
) %>%
  plotly::ggplotly(tooltip = "text")


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
  plotly::ggplotly(tooltip = "text") 


"Hopital du Personnell du Kolowezi"
"kz eThekwini Metropolitan Municipality Sub"