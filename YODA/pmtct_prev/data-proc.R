source("R/datim_geo.R")
source("R/worldpop.R")
library(ICPIutilities)
library(tidyverse)
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


extract_pmtct_data <- function(mer_data_source){
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
      
      dat <- dat %>% filter(indicator == "PMTCT_STAT",
                            standardizeddisaggregate == "Modality/Age/Sex/Result")
      if(nrow(dat) == 0){
        return(NULL)
      }
      dat$modality <- "PMTCT ANC"
      dat$indicator_orig <- dat$indicator
      dat$indicator <- ifelse(dat$numeratordenom == "N","HTS_TST_POS","HTS_TST_NEG")
      dat$modality <- "PMTCT ANC"
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
        modality == "PMTCT ANC",
        indicator %in% c("HTS_TST_POS","HTS_TST_NEG")
      )
    if(nrow(dat) == 0){
      return(NULL)
    }
    dat1 <- dat %>% 
      group_by(
        orgunituid  ,             sitename       ,          region        ,          
        regionuid             ,   operatingunit ,           operatingunituid ,      
        countryname        ,      snu1          ,           snu1uid        ,         
        psnu             ,        psnuuid, communityuid,
        sitename, facilityuid, facility,
        ageasentered, indicator
      ) %>%
      summarise_at(
        vars(starts_with("fy2")),
        function(x) sum(x,na.rm=TRUE)
      ) %>%
      ungroup()
    
    dat1 <- dat1 %>% 
      ungroup() %>%
      pivot_longer(
        cols = starts_with("fy2"),
        names_to = "fyid",
        values_to = "count",
        values_drop_na = TRUE
      ) %>%
      mutate(
        fiscal_year = as.numeric(str_sub(fyid,3,6)),
        quarter = paste0("qtr",str_sub(fyid,8))
      ) %>%
      filter(count > 0) %>%
      mutate(time = fiscal_year  + 
               (as.numeric(stringr::str_remove_all(quarter,"qtr"))-1)/4)
  }else{
    cat("Processing MER Data\n")
    dat <- rename_official(dat)
    dat <- dat %>% 
      filter(
        standardizeddisaggregate == "Modality/Age/Sex/Result",
        modality == "PMTCT ANC",
        indicator %in% c("HTS_TST_POS","HTS_TST_NEG")
      )
    if(nrow(dat) == 0){
      return(NULL)
    }
    dat1 <- dat %>% group_by(
      orgunituid  ,             sitename       ,          
      #region        ,               regionuid             ,   
      operatingunit ,           operatingunituid ,      
      countryname        ,      snu1          ,           snu1uid        ,         
      psnu             ,        psnuuid, communityuid,
      sitename, facilityuid, facility,
      ageasentered, fiscal_year, indicator) %>%
      summarise(
        qtr1=sum(qtr1,na.rm=TRUE),
        qtr2=sum(qtr2,na.rm=TRUE),
        qtr3=sum(qtr3,na.rm=TRUE),
        qtr4=sum(qtr4,na.rm=TRUE)
      )
    
    dat1 <- dat1 %>% 
      ungroup() %>%
      pivot_longer(
        cols = starts_with("qtr"),
        names_to = "quarter",
        values_to = "count",
        values_drop_na = TRUE
      ) %>%
      filter(count > 0) %>%
      mutate(time = fiscal_year  + 
               (as.numeric(stringr::str_remove_all(quarter,"qtr"))-1)/4)

  }
  if(nrow(dat1) < 3 || length(unique(dat1$indicator)) < 2){
    return(NULL)
  }
  #dat1$time <- dat1$time - max(dat1$time, na.rm=TRUE)
  df <- dat1 %>% 
    pivot_wider(
      names_from = indicator,
      values_from = count,
      values_fill = list(count=0)
    )
  names(df) <- tolower(names(df))
  
  # negs are actually totals
  if(data_source == "data_2016"){
    df$hts_tst_neg <- df$hts_tst_neg - df$hts_tst_pos
  }
  
  df
}


read_country <- function(country, data_dir, world_poly_dir){
  data_files <- list.files(paste0(data_dir, tolower(country)))
  
  mer_data_source <- paste0(
    data_dir,
    tolower(country),
    "/",
    data_files[stringr::str_detect(data_files, "(\\.txt|\\.rds)")][1]
  )
  world_pop_source <- paste0(
    data_dir,
    tolower(country),
    "/",
    data_files[stringr::str_detect(data_files, "\\.tif")][1]
  )
  
  df <- extract_pmtct_data(mer_data_source)
  
  cat("Reading country polygon\n")
  sp_country <- rgdal::readOGR(world_poly_dir)
  cn <- tolower(df$countryname[1])
  if(cn == "eswatini") cn <- "swaziland"
  country_poly <- sp_country[tolower(sp_country$NAME) == cn,]
  
  cat("Getting site location information\n")
  locations <- datim_get_locations(str_replace_all(country,"_"," "))
  da_locations <- site_locations(df, locations)
  
  cat("Hierarchical spatial clustering\n")
  da_locations$cluster_1 <- boxed_groups(da_locations, group_sizes[1])
  sp_cluster_1 <- create_sp_cluster(da_locations, group_sizes[1], country_poly)
  da_locations$cluster_2 <- boxed_groups(da_locations, group_sizes[2])
  sp_cluster_2 <- create_sp_cluster(da_locations, group_sizes[2], country_poly)
  da_locations$cluster_3 <- boxed_groups(da_locations, group_sizes[3])
  sp_cluster_3 <- create_sp_cluster(da_locations, group_sizes[3], country_poly)
  
  cat("Including worldpop info\n")
  wp <- raster::raster(world_pop_source)
  cluster <- parallel::makeCluster(pmin(10,parallel::detectCores()))
  da_locations$worldpop_10 <- world_pop_count_cluster(da_locations, wp, 10, cluster)
  da_locations$worldpop_50 <- world_pop_count_cluster(da_locations, wp, 50, cluster)
  parallel::stopCluster(cluster)
  #rm("wp")
  #rm("cluster")
  
  df_analysis <- merge(df, da_locations, all.x=TRUE)
  cat("Done\n")
  ret <- list(
    df_analysis = df_analysis,
    sp_cluster_1 = sp_cluster_1,
    sp_cluster_2 = sp_cluster_2,
    sp_cluster_3 = sp_cluster_3,
    da_locations = da_locations,
    mer_data_source = mer_data_source,
    world_pop_source = world_pop_source
  )
  ret
}



read_data <- function(mer_data_source, world_poly_dir, group_sizes, sub_country=NULL){
  
  df <- extract_pmtct_data(mer_data_source)
  if(is.null(df))
    return(NULL)
  cat("Reading country polygon\n")
  sp_country <- rgdal::readOGR(world_poly_dir, verbose=FALSE)
  
  if(!is.null(sub_country)){
    df <- df %>% filter(countryname == sub_country)
  }
  country <- unique(df$countryname) %>%
    tools::toTitleCase()
  if(length(country) > 1){
    res_list <- lapply(country,
                       function(cntry){
                         read_data(mer_data_source, 
                                               world_poly_dir, 
                                               group_sizes, 
                                               sub_country=cntry)
                       })
    return(list(data=res_list))
  }
  if(country == "Swaziland"){
    country <- "Eswatini"
  }
  #loc <- list()
  # cn <- tolower(df$countryname[1])
  # if(cn == "eswatini") cn <- "swaziland"
  # if(cn == "south sudan") cn <- "sudan"
  # if(cn == "tanzania") cn <- "united republic of tanzania"
  # if(cn == "vietnam") cn <- "viet nam"
  # country_poly <- sp_country[tolower(sp_country$NAME) == cn,]
  # if(nrow(country_poly) != 1)
  #   browser()
  # cat("Getting site location information\n")
  # locations <- datim_get_locations(str_replace_all(country,"_"," "))
  # da_locations <- site_locations(df, locations)
  # 
  # cat("Hierarchical spatial clustering\n")
  # da_locations$cluster_1 <- boxed_groups(da_locations, group_sizes[1])
  # sp_cluster_1 <- create_sp_cluster(da_locations, group_sizes[1], country_poly)
  # da_locations$cluster_2 <- boxed_groups(da_locations, group_sizes[2])
  # sp_cluster_2 <- create_sp_cluster(da_locations, group_sizes[2], country_poly)
  # da_locations$cluster_3 <- boxed_groups(da_locations, group_sizes[3])
  # sp_cluster_3 <- create_sp_cluster(da_locations, group_sizes[3], country_poly)
  # 
  # df_analysis <- merge(df, da_locations, all.x=TRUE)
  cat("Done\n")
  ret <- list(data=list(list(
    df_analysis = df,
    #sp_cluster_1 = sp_cluster_1,
    #sp_cluster_2 = sp_cluster_2,
    #sp_cluster_3 = sp_cluster_3,
    #da_locations = da_locations,
    mer_data_source = mer_data_source
  )))
  ret
}
