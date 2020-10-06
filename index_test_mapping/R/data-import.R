source("R/data-proc.R")
group_sizes <- c(50, 10, 5)

data_dir <- "country_data/2020q2"
world_poly_dir <- "../YODA/pmtct_prev/TM_WORLD_BORDERS-0.3"
out_dir <- "output/"

files <- list.files(data_dir)
files <- files[grepl("\\.txt",files)]
ld <- list()
for(f in files){
  print(f)
  mds <- paste0(data_dir,"/", f)
  res <- extract_index_data(mds)
  if(!is.null(res) ){
    ld[[length(ld)+1]] <- res
  }else{
    print("no data")
  }
}
ld_2020 <- ld
saveRDS(ld, file=paste0(out_dir, "ld_2020.rds"))



data_dir <- "country_data/2018q4"

files <- list.files(data_dir)
files <- files[grepl("\\.txt",files)]
ld <- list()
for(f in files){
  print(f)
  mds <- paste0(data_dir,"/", f)
  res <- extract_index_data(mds)
  if(!is.null(res) ){
    ld[[length(ld)+1]] <- res
  }else{
    print("no data")
  }
}
ld_2018 <- ld
saveRDS(ld, file=paste0(out_dir, "ld_2018.rds"))

ld <- c(
  lapply(ld_2020, function(x) x[x$fiscal_year > 2018,]),
  ld_2018)


df_analysis <- ld %>% 
  bind_rows() 
df_analysis$countryname[df_analysis$countryname == "Swaziland"] <- "Eswatini"


sp_country <- rgdal::readOGR(world_poly_dir)
countries <- unique(df_analysis$countryname)
dal <- list()
sp1 <- list()
sp2 <- list()
sp3 <- list()
for(country in countries){
  if(country == "_Military Caribbean Region"){
    next
  }
  print(country)
  cn <- tolower(country)
  if(cn == "eswatini") cn <- "swaziland"
  if(cn == "south sudan") cn <- "sudan"
  if(cn == "tanzania") cn <- "united republic of tanzania"
  if(cn == "vietnam") cn <- "viet nam"
  if(cn == "laos") cn <- "lao people's democratic republic"
  if(cn == "trinidad & tobago") next#cn <- "trinidad and tobago"
  
  country_poly <- sp_country[tolower(sp_country$NAME) == cn,]
  bb <- sp::bbox(country_poly)
  locations <- datim_get_locations(str_replace_all(country,"_"," "))
  tmp <- df_analysis %>%
    filter(
      countryname == country,
      !is.na(facilityuid)
    )
  da_locations <- site_locations(
    tmp, 
    locations)
  da_locations <- da_locations %>%
    filter(location_mode != "median_country", location_mode != "psnuid")
  if(nrow(da_locations) == 0){
    next
  }
  bb[,2] <- pmax(bb[,2], .00001 + c(max(da_locations$longitude),max(da_locations$latitude)))
  bb[,1] <- pmin(bb[,1], -.00001 + c(min(da_locations$longitude),min(da_locations$latitude)))
  cat("Hierarchical spatial clustering\n")
  da_locations$cluster_1 <- boxed_groups(
    da_locations, 
    group_sizes[1],
    lat_bd = bb[2,],
    long_bd = bb[1,],
    lat=TRUE
  ) %>% as.numeric()
  sp_cluster_1 <- create_sp_cluster(
    da_locations,
    group_sizes[1], 
    country_poly,
    bb,
    lat=TRUE)
  da_locations$cluster_2 <- boxed_groups(
    da_locations, 
    group_sizes[2],
    lat_bd = bb[2,],
    long_bd = bb[1,],
    lat=TRUE
  ) %>% as.numeric()
  sp_cluster_2 <- create_sp_cluster(
    da_locations,
    group_sizes[2], 
    country_poly,
    bb,
    lat=TRUE)
  da_locations$cluster_3 <- boxed_groups(
    da_locations, 
    group_sizes[3],
    lat_bd = bb[2,],
    long_bd = bb[1,],
    lat=TRUE
  ) %>% as.numeric()
  sp_cluster_3 <- create_sp_cluster(
    da_locations,
    group_sizes[3], 
    country_poly,
    bb,
    lat=TRUE)
  sp_cluster_1$countryname <- sp_cluster_2$countryname <- 
    sp_cluster_3$countryname <- da_locations$countryname <- country
  dal[[country]] <- da_locations
  sp1[[country]] <- sp_cluster_1
  sp2[[country]] <- sp_cluster_2
  sp3[[country]] <- sp_cluster_3
}
da_locations <- bind_rows(dal)
sp_cluster_1 <- do.call(rbind, sp1)
sp_cluster_2 <- do.call(rbind, sp2)
sp_cluster_3 <- do.call(rbind, sp3)


df_analysis <- merge(df_analysis, da_locations, all.x=TRUE)
df_analysis <- df_analysis %>% filter(!is.na(cluster_1))

df_analysis$cluster_1_id <- paste0(df_analysis$countryname,"_",df_analysis$cluster_1) %>%
  as.factor() %>%
  as.integer()
df_analysis$cluster_2_id <- paste0(df_analysis$countryname,"_",df_analysis$cluster_2) %>%
  as.factor() %>%
  as.integer()
df_analysis$cluster_3_id <- paste0(df_analysis$countryname,"_",df_analysis$cluster_3) %>%
  as.factor() %>%
  as.integer()

df_analysis$time <- df_analysis$fiscal_year + 
  (as.numeric(substr(df_analysis$quarter,4,4))-1)/4

df_analysis$time_ind <- as.integer((df_analysis$time - min(df_analysis$time))*4 + 1)
df_analysis$country_id <- as.integer(as.factor(df_analysis$countryname))
tmp <- with(df_analysis,paste(sitename, operatingunit, operatingunituid, countryname, snu1,
                              snu1uid, psnu,psnuuid, snuprioritization, typemilitary,
                              community, communityuid, facility, facilityuid, sitetype,
                              sep="_"))
df_analysis$site_id_txt <- tmp
df_analysis$site_id <- as.integer(as.factor(tmp))

saveRDS(df_analysis, file=paste0(out_dir, "df_analysis.rds"))
saveRDS(da_locations, file=paste0(out_dir, "da_locations.rds"))
saveRDS(sp_cluster_1, file=paste0(out_dir, "sp_cluster_1.rds"))
saveRDS(sp_cluster_2, file=paste0(out_dir, "sp_cluster_2.rds"))
saveRDS(sp_cluster_3, file=paste0(out_dir, "sp_cluster_3.rds"))





