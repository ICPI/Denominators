library(datapackr)
library(tidyverse)
library(geojsonio)
library(geojson)
library(sf)



datim_get_locations <- function(country){
  datim_logged_in <<- loginToDATIM("datim_cred.json")
  
  datim_countries <- api_call("organisationUnits") %>%
    api_filter("organisationUnitGroups.name:eq:Country") %>%
    api_get()
  
  country_uid <- datim_countries[datim_countries$displayName == country,1]
  
  locations <- api_call("organisationUnits") %>%
    api_fields("id,geometry,organisationUnitGroups,displayName") %>%
    api_filter(paste0("ancestors.id:eq:",country_uid)) %>%
    api_get()
  
  unit_groups <- api_call("organisationUnitGroups") %>%
    api_get()
  
  ug <- sapply(locations$organisationUnitGroups, function(x) x[[1]][1])
  locations$unit_group_1 <- unit_groups$displayName[match(ug, unit_groups$id)]
  ug <- sapply(locations$organisationUnitGroups, function(x) x[[1]][2])
  locations$unit_group_2 <- unit_groups$displayName[match(ug, unit_groups$id)]
  ug <- sapply(locations$organisationUnitGroups, function(x) x[[1]][3])
  locations$unit_group_3 <- unit_groups$displayName[match(ug, unit_groups$id)]
  
  locations$latitude <- NA
  locations$longitude <- NA
  for(i in 1:nrow(locations)){
    coord <- locations$geometry.coordinates[[i]]
    coord_type <- locations$geometry.type[[i]]
    if(is.null(coord) || is.na(coord) || is.na(coord_type))
      next
    if(is.list(coord) && coord_type != "MultiPolygon"){
      #message("Coercing to MultiPolygon")
      coord_type <- "MultiPolygon"
    }
    if(!is.list(coord) && coord_type == "MultiPolygon"){
      #message("Coercing away from MultiPolygon")
      if(length(dim(coord)) == 3)
        coord_type <- "Polygon"
      else
        coord_type <- "Point"
    }
    if(coord_type == "Polygon"){
      if(length(dim(coord)) == 3) 
        cc <- coord[1, , ]
      coord <- cc %>% 
        as.data.frame() %>% 
        geojsonio::geojson_list(lon = "V1",lat="V2") %>% 
        geojsonio::geojson_sf() %>% 
        st_coordinates() %>% 
        st_linestring() %>% 
        st_centroid() %>% 
        st_coordinates() %>%
        as.numeric()
    }else if(coord_type == "MultiPolygon"){
      coord <- rlang::flatten(coord)
      c1 <- list()
      for(j in 1:length(coord)){
        cc <- coord[[j]]
        if(length(dim(cc)) == 3) 
          cc <- cc[1, , ]
        #tryCatch({
        c1[[j]] <- cc %>% as.data.frame() %>% 
          geojsonio::geojson_list(lon = "V1",lat="V2") %>% 
          geojsonio::geojson_sf() %>% 
          st_coordinates() %>% 
          st_linestring() %>% 
          st_centroid() %>% 
          st_coordinates() %>%
          as.numeric()
        #}, error= function(xx) browser())
      }
      coord <- c(median(as.numeric(as.data.frame(c1)[1,])),
                 median(as.numeric(as.data.frame(c1)[2,])))
    }else if(coord_type != "Point"){
      warning(paste("Unknown geometry",coord_type))
      next
    }
      
    locations$longitude[i] <- coord[1]
    locations$latitude[i] <- coord[2]
  }
  locations
}


site_locations <- function(dat_analysis, locations){
  locations <- locations %>% filter(!is.na(latitude), !is.na(longitude))
  da_locations <- dat_analysis %>% group_by(sitename,facilityuid,psnuuid,communityuid) %>% summarise(n=n()) %>% select(-n)
  da_locations$longitude <- NA
  da_locations$latitude <- NA
  da_locations$location_mode <- NA
  
  # Find best location match
  for(i in 1:nrow(da_locations)){
    s <- locations[locations$id == da_locations$facilityuid[i],]
    if(is.na(da_locations$facilityuid[i]))
      next
    if(nrow(s) > 0){
      if(nrow(s)!= 1) browser()
      da_locations$longitude[i] <- s$longitude
      da_locations$latitude[i] <- s$latitude
      da_locations$location_mode[i] <- "facilityuid"
      next
    }
    s <- locations[locations$displayName == da_locations$sitename[i],]
    if(nrow(s) == 1){
      da_locations$longitude[i] <- s$longitude
      da_locations$latitude[i] <- s$latitude
      da_locations$location_mode[i] <- "sitename"
      next
    }
    s <- locations[locations$id == da_locations$communityuid[i],]
    if(nrow(s) > 0){
      da_locations$longitude[i] <- s$longitude
      da_locations$latitude[i] <- s$latitude
      da_locations$location_mode[i] <- "communityuid"
      next
    }
    s <- locations[locations$id == da_locations$psnuuid[i],]
    if(nrow(s) > 0){
      da_locations$longitude[i] <- s$longitude
      da_locations$latitude[i] <- s$latitude
      da_locations$location_mode[i] <- "psnuuid"
      next
    }
  }
  
  # if no location data on psnu/community/facility use median of known locations in the area
  da_locations_comp <- na.omit(as.data.frame(da_locations))
  for(i in 1:nrow(da_locations)){
    if(!is.na(da_locations$latitude[i]))
      next
    s <- da_locations_comp[da_locations_comp$communityuid == da_locations$communityuid[i],]
    if(nrow(s) > 0){
      da_locations$longitude[i] <- median(s$longitude)
      da_locations$latitude[i] <- median(s$latitude)
      da_locations$location_mode[i] <- "median_communityuid"
      next
    }
    
    s <- da_locations_comp[da_locations_comp$psnuuid == da_locations$psnuuid[i],]
    if(nrow(s) > 0){
      da_locations$longitude[i] <- median(s$longitude)
      da_locations$latitude[i] <- median(s$latitude)
      da_locations$location_mode[i] <- "median_psnuuid"
      next
    }
  }
  
  # fall back to country median
  miss <- is.na(da_locations$latitude)
  if(any(miss)){
    da_locations$longitude[miss] <- median(da_locations_comp$longitude)
    da_locations$latitude[miss] <- median(da_locations_comp$latitude)
    da_locations$location_mode[miss] <- "median_country"
  }
  da_locations
}



box_groupings <- function(latlon_sub, lat=FALSE, group_size = 10, 
                          lat_bd = NULL, long_bd=NULL, depth=1, max_depth=200){
  if(is.null(lat_bd)){
    lat_bd <- c(min(latlon_sub$latitude)- .00000001, max(latlon_sub$latitude)+.0000001)
  }
  if(is.null(long_bd)){
    long_bd <- c(min(latlon_sub$longitude)- .00000001, max(latlon_sub$longitude)+.0000001)
  }
  if(nrow(latlon_sub) < group_size*2 || depth >= max_depth){
    return(cbind(lat_lower=lat_bd[1], lat_upper=lat_bd[2], long_lower=long_bd[1], long_upper=long_bd[2]))
  }
  if(lat){
    m <- median(latlon_sub$latitude)
    latlon_sub[latlon_sub$latitude < m,]
    v1 <- box_groupings(latlon_sub[latlon_sub$latitude < m,], 
                        !lat,
                        group_size,
                        c(lat_bd[1], m),
                        long_bd,
                        depth=depth + 1,
                        max_depth=max_depth)
    v2 <- box_groupings(latlon_sub[latlon_sub$latitude >= m,],
                        !lat,
                        group_size,
                        c(m, lat_bd[2]),
                        long_bd,
                        depth=depth + 1,
                        max_depth=max_depth)
  }else{
    m <- median(latlon_sub$longitude)
    v1 <- box_groupings(latlon_sub[latlon_sub$longitude < m,], 
                        !lat,
                        group_size,
                        lat_bd,
                        c(long_bd[1], m),
                        depth=depth + 1,
                        max_depth=max_depth)
    v2 <- box_groupings(latlon_sub[latlon_sub$longitude >= m,],
                        !lat,
                        group_size,
                        lat_bd,
                        c(m, long_bd[2]),
                        depth=depth + 1,
                        max_depth=max_depth)    
  }
  rbind(v1,v2)
}

boxed_groups <- function(da_locations, group_size, lat_bd=NULL, long_bd=NULL, lat=FALSE){
  latlon_sub <- da_locations %>% filter(!is.na(latitude), ! is.na(longitude))
  
  
  nsites <- nrow(latlon_sub)
  latlon_sub$longitude <- latlon_sub$longitude + .000000001*(1:nsites) / nsites
  latlon_sub$latitude <- latlon_sub$latitude + .000000001*(1:nsites) / nsites
  
  bg <- box_groupings(
    latlon_sub,
    lat=lat,
    group_size = group_size,
    lat_bd=lat_bd,
    long_bd=long_bd)
  latlon_sub$cluster_group <- NA
  for(i in 1:nrow(bg)){
    latlon_sub[latlon_sub$latitude >= bg[i,"lat_lower"] &
                 latlon_sub$latitude < bg[i,"lat_upper"] &  
                 latlon_sub$longitude >= bg[i,"long_lower"] &
                 latlon_sub$longitude < bg[i,"long_upper"],
               "cluster_group"] <- i
  }
  df <- merge(da_locations, latlon_sub %>% select(-latitude,-longitude), all.x=TRUE)
  cluster_group <- df$cluster_group
  cluster_group[is.na(cluster_group)] <- "unknown_group"
  as.character(cluster_group)
}


