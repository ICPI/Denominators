source("../YODA/R/datim_geo.R")
library(ICPIutilities)
library(tidyverse)

extract_index_data <- function(mer_data_source){
  cat("Reading MER Data\n")
  tr <- try({
    dat <- ICPIutilities::read_msd(mer_data_source, remove_txt = FALSE, save_rds = FALSE)
    data_source <- "data_2020"
  }, silent=TRUE)
  if(inherits(tr,"try-error")){
    dat <- vroom::vroom(mer_data_source, delim = "\t", col_types = c(.default = "c"))
    dat <- dat %>% 
      dplyr::mutate_at(dplyr::vars(dplyr::starts_with("FY2")), ~as.double(.)) %>%
      dplyr::mutate_if(is.character, ~dplyr::na_if(., 
                                                   ""))
    dat <- dat %>%
      select(-dplyr::ends_with("APR")) %>%
      select(-dplyr::ends_with("TARGETS"))
    names(dat) <- tolower(names(dat))
    
    if(!("standardizeddisaggregate" %in% names(dat))){
      dat$standardizeddisaggregate <- ifelse(
        dat$disaggregate %in% c("Known/New/Age","Age"),
        "Modality/Age/Sex/Result",
        "")
      dat$ageasentered <- dat$age
      
      dat <- dat %>% filter(standardizeddisaggregate == "Modality/Age/Sex/Result")
      if(nrow(dat) == 0){
        return(NULL)
      }
      dat$indicator_orig <- dat$indicator
      dat$indicator <- ifelse(dat$numeratordenom == "N","HTS_TST_POS","HTS_TST_NEG")
      data_source <- "data_2016"
      dat$sitename <- "unknown"
      dat$snu1uid <- "unknown"
      dat$facility <- "unknown"
    }else{
      data_source <- "data_2018"
    }
    
    dat <- dat %>% 
      filter(
        standardizeddisaggregate == "Modality/Age/Sex/Result",
        indicator %in% c("HTS_TST_POS","HTS_TST_NEG")
      )
    
    if(nrow(dat) == 0){
      return(NULL)
    }
    datt <- dat %>%
      pivot_longer(
        cols = starts_with("fy"),
        values_to = "count",
        names_to = "year_qtr",
        values_drop_na = TRUE
      ) %>%
      mutate(
        fiscal_year = as.numeric(substr(year_qtr,3,6)),
        quarter = paste0("qtr",substr(year_qtr,8,9)),
        indicator = tolower(indicator)
      )
    
  }else{
    cat("Processing MER Data\n")
    dat <- rename_official(dat)

    datt <- dat %>% 
      filter(
        standardizeddisaggregate == "Modality/Age/Sex/Result",
        indicator %in% c("HTS_TST_POS","HTS_TST_NEG")
      ) %>%
      mutate(
        indicator = tolower(indicator)
      ) %>%
      ungroup() %>%
      pivot_longer(
        cols = starts_with("qtr"),
        names_to = "quarter",
        values_to = "count",
        values_drop_na = TRUE
      ) 
  }
  if(nrow(datt) == 0){
    return(NULL)
  }
  datt <- datt %>%
    pivot_wider(
      names_from = indicator,
      values_from = count,
      values_fill = list(count=0)
    ) %>%
    group_by(
      sitename, operatingunit, operatingunituid, countryname, snu1,
      snu1uid, psnu,psnuuid, snuprioritization, typemilitary,
      community, communityuid, facility, facilityuid, sitetype, 
      modality, fiscal_year, quarter
    ) %>%
    summarise(
      hts_tst_pos = if(exists("hts_tst_pos")) sum(hts_tst_pos) else 0,
      hts_tst_neg = if(exists("hts_tst_neg")) sum(hts_tst_neg) else 0
    ) %>%
    ungroup() 
  
  modalities <- unique(datt$modality)
  index_modalities <- intersect(c("Index","IndexMod"),unique(datt$modality))
  non_index_modalities <- setdiff(modalities, index_modalities)
  
  datt <- datt %>%
    pivot_wider(
      names_from = modality,
      values_from = c(hts_tst_pos, hts_tst_neg),
      values_fill = list(hts_tst_pos=0, hts_tst_neg=0)
    )
  
  datt$hts_tst_neg_non_index <- rowSums(datt[paste0("hts_tst_neg_", non_index_modalities)])
  datt$hts_tst_pos_non_index <- rowSums(datt[paste0("hts_tst_pos_", non_index_modalities)])
  datt$hts_tst_tot_non_index <- datt$hts_tst_neg_non_index + datt$hts_tst_pos_non_index
  if(length(index_modalities) != 0){
    datt$hts_tst_neg_index <- rowSums(datt[paste0("hts_tst_neg_", index_modalities)])
    datt$hts_tst_pos_index <- rowSums(datt[paste0("hts_tst_pos_", index_modalities)])
  }else{
    datt$hts_tst_neg_index <- 0
    datt$hts_tst_pos_index <- 0
  }
  datt$hts_tst_tot_index <- datt$hts_tst_neg_index + datt$hts_tst_pos_index
  

  datt
}




boxes_to_polygons <- function(boxes, country_poly){
  boxes <- as.data.frame(boxes)
  row.names(boxes) <- as.character(1:nrow(boxes))
  l <- list()
  for(i in 1:nrow(boxes)){
    l[[i]] <- rbind(
      as.numeric(boxes[i,c("long_lower","lat_lower")]),
      as.numeric(boxes[i,c("long_lower","lat_upper")]),
      as.numeric(boxes[i,c("long_upper","lat_upper")]),
      as.numeric(boxes[i,c("long_upper","lat_lower")])
    ) %>% 
      sp::Polygon() %>% 
      list() %>%
      sp::Polygons(ID = row.names(boxes)[i])
  }
  polys <- sp::SpatialPolygons(l)
  sp_boxes <- sp::SpatialPolygonsDataFrame(polys, boxes)
  sp::proj4string(sp_boxes) <- sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  sp_boxes <- raster::intersect(sp_boxes, country_poly)
  sp_boxes
}


create_sp_cluster <- function(da_locations, group_size, country_poly, bb, lat=FALSE){
  latlon_sub <- da_locations %>% ungroup() %>% 
    filter(!is.na(latitude), ! is.na(longitude)) %>%
    select(facilityuid, latitude, longitude) %>% ungroup()
  
  nsites <- nrow(latlon_sub)
  latlon_sub$longitude <- latlon_sub$longitude + .000000001*(1:nsites) / nsites
  latlon_sub$latitude <- latlon_sub$latitude + .000000001*(1:nsites) / nsites
  
  #bb <- sp::bbox(country_poly)
  #bb[,2] <- pmax(bb[,2], .00001 + c(max(da_locations$longitude),max(da_locations$latitude)))
  #bb[,1] <- pmin(bb[,1], -.00001 + c(min(da_locations$longitude),min(da_locations$latitude)))
  
  bg <- box_groupings(
    latlon_sub,
    group_size = group_size,
    lat_bd = bb[2,],
    long_bd = bb[1,],
    lat=lat
  )
  latlon_sub$cluster_group <- NA
  latlon_sub$lat_lower <- NA
  latlon_sub$lat_upper <- NA
  latlon_sub$long_lower <- NA
  latlon_sub$long_upper <- NA
  for(i in 1:nrow(bg)){
    sub <- latlon_sub$latitude >= bg[i,"lat_lower"] &
      latlon_sub$latitude < bg[i,"lat_upper"] &  
      latlon_sub$longitude >= bg[i,"long_lower"] &
      latlon_sub$longitude < bg[i,"long_upper"]
    latlon_sub[sub, "cluster_group"] <- i
    latlon_sub[sub, "lat_lower"] <- bg[i,"lat_lower"]
    latlon_sub[sub, "lat_upper"] <- bg[i,"lat_upper"]
    latlon_sub[sub, "long_lower"] <- bg[i,"long_lower"]
    latlon_sub[sub, "long_upper"] <- bg[i,"long_upper"]
  }
  boxes <- latlon_sub
  boxes_to_polygons(boxes, country_poly)
}
