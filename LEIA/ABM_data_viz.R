# combined_df is the output from the ABM model

dfx <- read_rds("abm_output.rds")

sapply(dfx, class)

dfx$fl_treated <- as.numeric(dfx$fl_treated)


# Sys.Date()
dx <- as.character(format(Sys.time(), "%Y %b %d"))
t <- as.character(format(Sys.time(), "%H %M %S" ))
tm <- str_replace_all(t, "[ ]", "_")
dt <- str_replace_all(dx, "[ ]", "")

# exporting the raw agent-level dataset after ABM run
write.csv(agentmapf, 
          paste("Output/", dt, "_agent_level_psnu_", tm, ".csv", sep=""), na="", row.names = F)

# Creating color palatte for polygons
# Adding sitetype to the sitekey 
sitekeytype <- left_join(sitekey, (geodata %>% select(-sitetype) %>% unique()))

agentmapf <- left_join(dfx, sitekeytype)


# Creating site-level dataset with updated coverage numbers
link_dfx <- agentmapf %>% filter(assigned == 1) %>% 
  mutate(fl_treated = as.numeric(fl_treated)) %>% 
  group_by(siteid, fl_treated) %>% 
  summarise(treated = sum(assigned, na.rm=T)) %>% 
  ungroup() %>% 
  mutate(linktype = if_else(siteid==fl_treated, "intr", "extr")) %>% 
  group_by(siteid, fl_treated, linktype) %>% 
  summarise(treated = sum(treated, na.rm=T)) %>% 
  ungroup() %>% 
  spread(linktype, treated) 

dummy_link <- link_dfx[FALSE, FALSE] %>% 
  mutate(extr = NA,
         intr = NA)

link_df <- bind_rows(link_dfx,dummy_link) %>% 
  mutate_if(is.numeric, list(~replace(., is.na(.), 0 ))) %>% 
  rowwise() %>% 
  mutate(tot_tx = sum(extr, intr, na.rm=T)) %>% 
  ungroup()


unlink_df <- agentmapf %>% filter(assigned == 0) %>%   
  group_by(siteid) %>%     
  summarise(untreated = sum(not_assigned, na.rm=T)) %>%
  ungroup()



alllink <- bind_rows(link_df, unlink_df)




# Create corresponding APR treatment and testing datasets
txhts <- df2y %>% 
  filter(timept %in% q_vec) %>% 
  filter(agesex %in% agesexvec[7:8]) %>% 
  select(uid, TX_NEW, HTS_TST_POS) %>% 
  group_by(uid) %>% 
  summarize_all(list(~sum), na.rm=T) %>% 
  ungroup()

txhtsx <- left_join(txhts, sitekey) %>% select(-uid)

# link_dfx <- bind_rows(link_df, txhtsx)

all_df <- bind_rows(alllink, txhtsx)

# Adding back the orguniuid
# link_f <- left_join(link_dfx, sitekey)

alllink_f <- left_join(all_df, sitekey)

f_df <- left_join(alllink_f, geodata) 

# adding info for the treatment site
names(f_df)  <-   c("mod_tst_id",
                    "mod_rx_id",
                    "ext_rx",
                    "int_rx",
                    "tot_rx",
                    "untreated",
                    "tx_new",
                    "hts_pos",
                    "uid_tst",
                    "sitename_tst",
                    "sitetype_tst",
                    "psnu_tst",
                    "psnuuid_tst",
                    "snu1_tst",
                    "snu1uid_tst",
                    "lng_tst",
                    "lat_tst")


sitekey2 <- sitekey %>% rename(uid_rx = uid,
                               mod_rx_id = siteid)  

geodata2 <- geodata %>% 
  rename(uid_rx = uid, 
         sitename_rx = sitename, 
         sitetype_rx = sitetype, 
         psnu_rx = psnu, 
         psnuuid_rx = psnuuid,
         snu1_rx = snu1, 
         snu1uid_rx = snu1uid,
         lng_rx = Longitude,
         lat_rx = Latitude) %>% 
  unique()

fdf <- left_join(f_df, sitekey2)

fdfx <- left_join(fdf, geodata2) 


# Sys.Date()
dx <- as.character(format(Sys.time(), "%Y %b %d"))
t <- as.character(format(Sys.time(), "%H %M %S" ))
tm <- str_replace_all(t, "[ ]", "_")
dt <- str_replace_all(dx, "[ ]", "")

write.csv(fdfx, 
          paste("Output/", dt, "_AdjLinkage_final_psnu_", tm, ".csv", sep=""), na="", row.names = F)


saveRDS(fdfx, "full_site.rds")

# calculate adjusted and proxy linkage values at SNU1 level
snu_link <- fdfx %>% 
  select(snu1_tst, snu1uid_tst,
         tot_rx, tx_new,	hts_pos) %>% 
  group_by(snu1_tst, snu1uid_tst) %>% 
  summarize_all(list(~sum), na.rm=T) %>% 
  mutate(prox_link = round((tx_new/hts_pos)*100,0),
         abm_link = round((tot_rx/hts_pos)*100,0))


snu_link$snu1 <- factor(snu_link$snu1_tst, levels = snu_link$snu1_tst[order(snu_link$snu1_tst)])

library(plotly)

f <- list(
  family = "Arial",
  size = 18,
  color = "#7f7f7f"
)
x <- list(
  title = "% linkage",
  titlefont = f
)
y <- list(
  title = "SNU1",
  titlefont = f
)

p <- plot_ly(snu_link, color = I("gray80")) %>%
  add_segments(x = ~prox_link, xend = ~abm_link, y = ~snu1, yend = ~snu1, showlegend = FALSE) %>%
  add_markers(x = ~prox_link, y = ~snu1, name = "Proxy", color = I("#B5B867")) %>%
  add_markers(x = ~abm_link, y = ~snu1, name = "ABM", color = I("#335B8E")) %>% 
  layout(xaxis = x, yaxis = y)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
region_shpx <- readRDS("Nigeria_State_shp.rds")
# Cleaning up Region shape files, taking out NA values
region_shp <- region_shpx[!is.na(region_shpx$NAME),]

# calculate adjusted and proxy linkage values at SNU1 level
psnulink <- fdfx %>% 
  select(psnu_tst, psnuuid_tst,
         tot_rx, tx_new,	hts_pos) %>% 
  group_by(psnu_tst, psnuuid_tst) %>% 
  summarize_all(list(~sum), na.rm=T) %>% 
  ungroup() %>% 
  mutate(prox_link = round((tx_new/hts_pos)*100,0),
         abm_link = round((tot_rx/hts_pos)*100,0)) %>% 
  # Remove NA and NaNs
  mutate_if(is.double, list(~replace(., is.na(.), NA_real_))) %>% 
  mutate_if(is.double, list(~replace(., is.nan(.), NA_real_))) %>% 
  mutate_if(is.double, list(~replace(., .>999999999, NA_real_)))
              

shape_df <- merge(psnu_shp, psnulink, by.x = "uid", by.y = "psnuuid_tst")

c_abm <- colorRampPalette(c('#cc5234', '#b5b867', '#6ca18f'))(46)
pal_abm <- colorNumeric(palette = c_abm, domain = shape_df$abm_link)

c_prox <- colorRampPalette(c('#cc5234', '#b5b867', '#6ca18f'))(72)
pal_prox <- colorNumeric(palette = c_prox, domain = shape_df$prox_link)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Visualizing PSNU-level ABM linkage
psnu_map_abm <- leaflet(data=shape_df) %>%
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  addPolygons(fillColor = ~pal_abm(abm_link), 
              color='white', weight=1, opacity=0.9, fillOpacity = 0.9) %>% 
  addPolygons(data=region_shp,fillColor = "grey", 
              color='black', weight=1.5, opacity=0.4, fillOpacity = 0.01) %>% 
  addLegend(pal = pal_abm, values = ~abm_link,
            labFormat = labelFormat(suffix = '%', between = '% - '),
            opacity = 0.9, title = 'ABM Linkage',position = "bottomright") %>%
  addScaleBar()

psnu_map_prox <- leaflet(data=shape_df) %>%
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  addPolygons(fillColor = ~pal_prox(prox_link), 
              color='white', weight=1, opacity=0.5, fillOpacity = 0.9) %>% 
  addLegend(pal = pal_prox, values = ~prox_link,
            labFormat = labelFormat(suffix = '%', between = '% - '),
            opacity = 0.9, title = 'Proxy Linkage',position = "bottomright") %>%
  addScaleBar()


shape_df$psnu_label <- if_else(is.na(shape_df$psnu_tst), "", 
                               if_else(is.na(shape_df$abm_link), shape_df$psnu_tst, 
                                       paste(shape_df$psnu_tst, ": ", shape_df$abm_link, "%")))

psnu_map_abm <- leaflet(data=shape_df) %>%
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  addPolygons(fillColor = ~pal_abm(abm_link), 
              color='white', weight=1, opacity=0.9, fillOpacity = 0.9,
              label=~psnu_label,
              labelOptions = labelOptions(
                noHide = T, 
                textOnly = TRUE,
                textsize = "12px",
                direction = "center")) %>% 
  addPolygons(data=region_shp,fillColor = "grey", 
              color='black', weight=1.5, opacity=0.4, fillOpacity = 0.01) %>% 
  addLegend(pal = pal_abm, values = ~abm_link,
            labFormat = labelFormat(suffix = '%', between = '% - '),
            opacity = 0.9, title = 'ABM Linkage', position = "bottomright") %>%
  addCircleMarkers(data=agentmapf, lng=~lng, lat=~lat, radius=1, opacity=1,
                   label=~paste(pt_id, siteid, sep="_"), color=~pal2(dftype),
                   labelOptions = labelOptions(noHide = F, direction = 'topright', 
                                               style = list("color" = "blue"))) %>% 
  addLegend(data=agentmapf, pal = pal2, values = ~dftype, group = "circles", position = "bottomright") %>%  
  addScaleBar()

unass_agents <- agentmapf %>% filter(not_assigned==1)

# Only unassigned agents, by community and facility

# Checking placement of sites in appropriate 
colfn3 <- colorRampPalette(c('#A379BB', 
                             'pink'))(2)
pal3 <- colorFactor(palette = colfn2, domain = unass_agents$sitetype)


psnu_map_abm <- leaflet(data=shape_df) %>%
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  addPolygons(fillColor = ~pal_abm(abm_link), 
              color='white', weight=1, opacity=0.9, fillOpacity = 0.9,
              label=~psnu_label,
              labelOptions = labelOptions(
                noHide = T, 
                textOnly = TRUE,
                textsize = "12px",
                direction = "center")) %>% 
  addPolygons(data=region_shp,fillColor = "grey", 
              color='black', weight=1.5, opacity=0.4, fillOpacity = 0.01) %>% 
  addLegend(pal = pal_abm, values = ~abm_link,
            labFormat = labelFormat(suffix = '%', between = '% - '),
            opacity = 0.9, title = 'ABM Linkage', position = "bottomright") %>%
  addCircleMarkers(data=agentmapf, lng=~lng, lat=~lat, radius=1, opacity=1,
                   label=~paste(pt_id, siteid, sep="_"), color=~pal2(dftype),
                   labelOptions = labelOptions(noHide = F, direction = 'topright', 
                                               style = list("color" = "blue"))) %>% 
  addLegend(data=agentmapf, pal = pal2, values = ~dftype, group = "circles", position = "bottomright") %>%  
  addScaleBar()



colfn2 <- colorRampPalette(c('#cc5234', '#b5b867', '#6ca18f'))(80)
pal2 <- colorNumeric(palette = colfn2, domain = shape_df$abm_link)

psnu_map2 <- leaflet(data=shape_df) %>%
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  addPolygons(fillColor = ~pal2(abm_link), 
              color='white', weight=1, opacity=0.5, fillOpacity = 0.9) %>% 
  addLegend(pal = pal2, values = ~abm_link,
            labFormat = labelFormat(suffix = '%', between = '% - '),
            opacity = 0.9, title = 'ABM Linkage',position = "bottomright") %>%
  addScaleBar()



library(colorspace)
colorx <- terrain_hcl(length(unique(psnu_shp$level5name)))

factpal <- colorFactor(colorx, psnu_shp$level5name)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Creating the unassigned agents map
state_popup <- paste0("<strong>PSNU: </strong>", 
                      psnu_shp$level5name)
# Checking placement of sites in appropriate 
colfn2 <- colorRampPalette(c('#335b8e', 
                             '#B2182B'))(2)
pal2 <- colorFactor(palette = colfn2, domain = age_10_14$dftype)


# Leaflet map to show unassigned agents on top of adjusted linkage
# Also adding treatment facilites icons
leaflet(psnu_shp) %>%   
  addPolygons(fillColor=~factpal(level5name),
              color='white', weight=1, opacity=.7, fillOpacity = 0.6, 
              popup = state_popup) %>% 
  addCircleMarkers(data=age_10_14, lng=~lng, lat=~lat, radius=1, opacity=1,
                   label=~paste(pt_id, siteid, sep="_"), color=~pal2(dftype),
                   labelOptions = labelOptions(noHide = F, direction = 'topright', 
                                               style = list("color" = "blue"))) %>% 
  addLegend(data=age_10_14, pal = pal2, values = ~dftype, group = "circles", position = "bottomright") 


# Adding where the treatment sites are present



# Adding sitetype to the sitekey 

agentpsnu <- unique(agentmapf$psnuuid)

newpgon <- psnu_shp[psnu_shp$uid 
                    %in% agentpsnu,] 


colorxx <- terrain_hcl(length(unique(newpgon$uid)))

factpalx <- colorFactor(colorxx, newpgon$uid)

# icons <- awesomeIcons(
#   icon = 'hospital-o',
#   iconColor = pal_p2(newpgon$uid),
#   library = 'fa',
#   markerColor = "white"
# )
# 
# icon_comm <- awesomeIcons(
#   icon = 'ambulance',
#   iconColor = pal_p2(newpgon$uid),
#   library = 'fa',
#   markerColor = "white"
# )

# Visualizing clusters on map
mapx <- leaflet(newpgon) %>%   
  addPolygons(data=psnu_shp, fillColor=NULL,
              color='grey', weight=.5, opacity=.6, fillOpacity = 0.4) %>%
  addPolygons(fillColor=~factpalx(newpgon$uid),
              color='white', weight=1, opacity=.7, fillOpacity = 0.4) %>%
  addCircleMarkers(data=agentmapf, lng=~lng, lat=~lat, radius=1, opacity=1,
                   label=~paste(pt_id, siteid, sep="_"), color=~pal2(dftype),
                   labelOptions = labelOptions(noHide = F, direction = 'topright', 
                                               style = list("color" = "blue"))) %>% 
  addLegend(data=agentmapf, pal = pal2, values = ~dftype, group = "circles", position = "bottomright") 






# Checking placement of sites in appropriate 
colfn <- colorRampPalette(c('#335b8e', 
                            '#6ca18f', 
                            '#b5b867',
                            '#cc5234', 
                            '#d9812c', 
                            '#948d79'))(length(unique(psnu_shp$level5name)))
pal <- colorFactor(palette = colfn, domain = psnu_shp$level5name)
state_popup <- paste0("<strong>PSNU: </strong>", 
                      psnu_shp$level5name)
# Checking placement of sites in appropriate 
colfn2 <- colorRampPalette(c('#335b8e', 
                             'red'))(2)
pal2 <- colorFactor(palette = colfn2, domain = age_10_14$dftype)

library(colorspace)
colorx <- terrain_hcl(length(unique(psnu_shp$level5name)))

factpal <- colorFactor(colorx, psnu_shp$level5name)


leaflet(psnu_shp) %>%   
  addPolygons(fillColor=~factpal(level5name),
              color='white', weight=1, opacity=.7, fillOpacity = 0.6, 
              popup = state_popup) %>% 
  
  addCircleMarkers(data=age_10_14, lng=~lng, lat=~lat, radius=1, opacity=1,
                   label=~paste(pt_id, siteid, sep="_"), color=~pal2(dftype),
                   labelOptions = labelOptions(noHide = F, direction = 'topright', 
                                               style = list("color" = "blue"))) %>% 
  addLegend(data=age_10_14, pal = pal2, values = ~dftype, group = "circles", position = "bottomright") 



finalmap <- bscols(mapx)

library(htmltools)

htmltools::save_html(finalmap, "Output/unassigned_XXX.html")

# %>%
#   addAwesomeMarkers(data=agentmapf, lng=~lng,lat=~lat, icon=icons,
#                     label=~paste(site_name, " (tx/hts:", TX_NEW, "/", HTS_TST_POS, ")", sep="")) %>% 
#   addAwesomeMarkers(data=node_comm, lng=~lng,lat=~lat, icon=icon_comm,
#                     label=~paste(site_name, " (tx/hts:", TX_NEW, "/", HTS_TST_POS, ")", sep=""))

# 
# # save r datasets
# save(linkg,file="linkg.Rda")
# save(pt_unass,file="pt_unass.Rda")
# save(total_unassigned,file="total_unassigned.Rda")
# save(total_assigned,file="total_assigned.Rda")
# save(fdfx,file="fdfx.Rda")



finalmap <- bscols(mapx)

library(htmltools)

htmltools::save_html(finalmap, "Output/unassigned_XXX.html")





# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# filtering for apr and calculating linkage at PSNU level



shape_df <- merge(civ_district, psnulink, by.x = "uid", by.y = "psnuuid_tst")

colfn <- colorRampPalette(c('#cc5234', '#b5b867', '#6ca18f'))(80)
pal <- colorNumeric(palette = colfn, domain = shape_df$prox_link)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Visualizing clusters on map
psnu_map <- leaflet(data=shape_df) %>%
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  addPolygons(fillColor = ~pal(prox_link), 
              color='white', weight=1, opacity=0.5, fillOpacity = 0.9) %>% 
  addLegend(pal = pal, values = ~prox_link,
            labFormat = labelFormat(suffix = '%', between = '% - '),
            opacity = 0.9, title = 'Proxy Linkage',position = "bottomright") %>%
  addScaleBar()


colfn2 <- colorRampPalette(c('#cc5234', '#b5b867', '#6ca18f'))(80)
pal2 <- colorNumeric(palette = colfn2, domain = shape_df$abm_link)

psnu_map2 <- leaflet(data=shape_df) %>%
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  addPolygons(fillColor = ~pal2(abm_link), 
              color='white', weight=1, opacity=0.5, fillOpacity = 0.9) %>% 
  addLegend(pal = pal2, values = ~abm_link,
            labFormat = labelFormat(suffix = '%', between = '% - '),
            opacity = 0.9, title = 'ABM Linkage',position = "bottomright") %>%
  addScaleBar()




label = ~paste(
  "Region: ", level4name, "/ ",
  "PSNU: ", name, " (", 
  format(tx_new, big.mark=",", scientific=FALSE),"/"
  format(hts_pos, big.mark=",", scientific=FALSE),
  ")", sep="")  
# highlight = highlightOptions(
# weight = 5,
# color = "#666",
# dashArray = "",
# fillOpacity = 0.8, bringToFront=F)
) %>%
  
  color='white', weight=1, opacity=.7, fillOpacity = 0.4)
