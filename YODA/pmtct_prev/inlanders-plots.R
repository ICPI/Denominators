library(tidyverse)
library(leaflet)
library(sp)

options(knitr.kable.NA = '')


out_dir <- "pmtct_prev/output/"
df_analysis <- readRDS(paste0(out_dir, "df_analysis.rds"))

fit_files <- list.files(paste0(out_dir,"fits"))

fits <- lapply(
  fit_files,
  function(f){
    cat(".")
    readRDS(paste0(out_dir,"fits/",f))
  }
)

df_result <- lapply(fits,function(x) x$df_result) 
df_result <- do.call(rbind,df_result)
df_raw <- lapply(fits,function(x){
  df <- x$df_raw
  df$countryname <- x$df_result$countryname[1]
  df
}) 
df_raw <- do.call(rbind,df_raw)
df_result$cl_id2 <- paste0(df_result$countryname,"_",df_result$cl_id)
df_raw$cl_id2 <- paste0(df_raw$countryname,"_",df_raw$cl_id)


#df_raw_plot <- df_raw[df_raw$time_ind == 21,]
#
#pal <- colorNumeric("Reds", domain=df_raw_plot$yield)
#m <- df_raw_plot %>%
#  leaflet() %>%
#  addProviderTiles(providers$CartoDB.Positron) %>%
#  addPolygons(
#    fillColor=~pal(yield),#pal(fitted),
#    weight=.25,
#    color="#ffffff",
#    fillOpacity = 1
#  )
#print(m)


time_map <- unique(df_analysis %>% select(time_ind, fiscal_year, quarter, time))

df_plot <- df_result 
df_plot@data <- df_plot@data %>%
  select(-hts_tst_pos,-hts_tst_tot) %>%
  left_join(df_raw@data) %>%
  left_join(time_map)

cl_id2s <- unique(df_plot$cl_id2)

popups <- sapply(cl_id2s, function(cl){
  tmp <- df_plot@data %>%
    filter(cl_id2 == cl) %>%
    mutate(
      hts_tst_pos = ifelse(hts_tst_tot < 5,NA, hts_tst_pos),
      yield = ifelse(hts_tst_tot < 5,NA, yield)*100,
      hts_tst_tot = ifelse(hts_tst_tot < 5,NA, hts_tst_tot),
      adjusted_yield = fitted*100
    ) %>%
    arrange(time_ind)
  out <- tmp %>% 
    select(fiscal_year, quarter, hts_tst_pos, hts_tst_tot, yield, adjusted_yield) %>% 
    knitr::kable(
      digits = 1,
      format="html", 
      caption = "PMTCT Yield") %>%
    as.character()
  out
})
df_plot@data <- df_plot@data %>%
  left_join(
    data.frame(cl_id2=cl_id2s,
               popup_html=popups)
  )


df_site_plot <- df_analysis %>%
  group_by(sitename, countryname, facility, facilityuid, fiscal_year, 
           quarter, time, longitude, latitude, location_mode, cluster_1, cluster_1_id, 
           cluster_2, cluster_2_id, cluster_3, cluster_3_id) %>%
  summarise(
    hts_tst_tot = sum(hts_tst_pos) + sum(hts_tst_neg),
    hts_tst_pos = sum(hts_tst_pos)) %>%
  filter(hts_tst_tot > 1)

sp_cluster_3 <- readRDS(paste0(out_dir, "sp_cluster_3.rds"))
tmp <- sp_cluster_3@data %>%
  group_by(countryname) %>%
  transmute(
    cl_id = {
      sp_cl <- data.frame(countryname,cluster_group)
      unq <- !duplicated(sp_cl)
      sp_cl <- sp_cl[unq,]
      id <- 1:nrow(sp_cl)
      id[match(cluster_group, sp_cl$cluster_group)]
    },
    cluster_3 = cluster_group
  ) %>%
  unique()
df_site_plot <- df_site_plot %>%
  ungroup() %>%
  left_join(tmp) %>%
  left_join(df_plot@data %>% select(countryname, time, cl_id, fitted))

fuid <- unique(df_site_plot$facilityuid)

site_popups <- sapply(fuid, function(fid){
  tmp <- df_site_plot %>%
    filter(facilityuid == fid) %>%
    mutate(
      hts_tst_pos = ifelse(hts_tst_tot < 2,NA, hts_tst_pos),
      yield = ifelse(hts_tst_tot < 2,NA, hts_tst_pos / hts_tst_tot)*100,
      hts_tst_tot = ifelse(hts_tst_tot < 2,NA, hts_tst_tot),
      adjusted_yield = fitted*100
    ) %>%
    arrange(time)
  out <- tmp %>% 
    select(fiscal_year, quarter, hts_tst_pos, hts_tst_tot, yield, adjusted_yield) %>% 
    knitr::kable(
      digits = 1,
      format="html", 
      caption = paste(tmp$facility[1], "(PMTCT Yield)")) %>%
    as.character()
  out
})
df_site_plot <- df_site_plot %>%
  left_join(
    data.frame(facilityuid=fuid,
               popup_html=site_popups)
  )

save("df_plot","df_site_plot", file="pmtct_prev/shiny_app/data/data.RData")


df_plot_sub <- df_plot[df_plot$time_ind == 21,]

pal <- colorNumeric("Reds", domain=c(0,df_plot_sub$fitted))
m <- df_plot_sub %>%
  leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(
    fillColor=~pal(fitted),#pal(fitted),
    weight=.25,
    color="#ffffff",
    fillOpacity = 1,
    popup = df_plot_sub$popup_html,
    popupOptions = popupOptions(maxWidth = 1000)
  ) %>%
  addLegend("bottomright", pal = pal, values = c(0,df_plot_sub$fitted),
            title = "Smoothed Adjusted Yield",
            bins=4,
            opacity = 1,
            labFormat = labelFormat(suffix = "%", transform=function(x) x*100)
  )
print(m)



df_site_plot_sub <- df_site_plot %>%
  filter(time == 2020.25, !is.na(fitted))
pal <- colorNumeric("Reds", domain=c(0,df_site_plot_sub$fitted))
m <- df_site_plot_sub %>%
  leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircles(
    lng=~longitude, 
    lat=~latitude,
    color=~pal(fitted),
    popup = df_site_plot_sub$popup_html,
    popupOptions = popupOptions(maxWidth = 1000)
    ) %>%
  addLegend("bottomright", pal = pal, values = c(0,df_site_plot_sub$fitted),
            title = "Smoothed Adjusted Yield",
            bins=4,
            opacity = 1,
            labFormat = labelFormat(suffix = "%", transform=function(x) x*100)
  )
print(m)


