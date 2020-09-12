
get_mer_file <- function(country){
  files <- list.files(tolower(country))
  mer_files <- files[stringr::str_starts(files,"MER") & 
                       (stringr::str_ends(files,"txt") | 
                          stringr::str_ends(files,"rds"))]
  dts <- sapply(strsplit(mer_files,"_"), function(x) x[7])
  dts <- as.numeric(strptime(dts,"%Y%m%d"))
  paste0(tolower(country),"/", mer_files[which.max(dts)])
}

get_worldpop_file <- function(country){
  files <- list.files(tolower(country))
  files <- files[stringr::str_ends(files,"tif")]  
  yr <- as.numeric(sapply(strsplit(files,"_|\\."), function(x) x[3]))
  paste0(tolower(country),"/",files[which.max(yr)])
}

get_spectrum_file <- function(country){
  file <- paste0(country,"/spectrum.xlsx")
  if(file.exists(file)) file else NULL
}

mer_data_source <- get_mer_file(country)
world_pop_source <- get_worldpop_file(country)
spectrum_data_source <- get_spectrum_file(country)


if(!exists("verbose")) verbose <- 1
vcat <- function(...) if(verbose) cat(...)

if(!exists("country")) country <- "Cote d'Ivoire"
if(!exists("max_diff")) max_diff <- 1.2
if(!exists("n_steps")) n_steps <- 4

pmtct_formula <- NULL
pmtct_re_formula <- NULL

index_formula <- cbind(hts_tst_pos_index, hts_tst_index - hts_tst_pos_index) ~ pediatric + 
  pmtct_lin_pred + log(worldpop_50 + 1) + 
  log((hts_tst_index + 1) / (hts_tst_pos_non_index + 1) ) + 
  I((hts_tst_pos_non_index==0))+#*log(hts_tst_index + 1)) +
  (1 | cluster_1 / cluster_3 / sitename)

index_ratio <- function(ratio, pediatric){
  ratio[is.na(ratio)] <- 0
  ratio[!pediatric & ratio < 1.5]  <-pmax(1.5, ratio[!pediatric & ratio < 1.5] * 1.4)
  ratio[pediatric & ratio < .4]  <-pmax(.4, ratio[pediatric & ratio < .4] * 1.4)
  ratio[!pediatric & ratio < .5] <- .5
  ratio[pediatric & ratio < .1] <- .1
  ratio[is.infinite(ratio)] <- 1.5
  ratio
}

allocation_lookback <- -Inf


frm <- NULL
time_cuts <- c(-0.75,  -0.5, -0.25)
max_increase <- Inf
total_tests_target = NULL
group_sizes <- c(100,30,10)
site_re_formula <- ~(1 | sitename / modality) + (1 | cluster_1 / cluster_2 / cluster_3)
if(country == "Cote d'Ivoire"){
  mer_data_source <- "cote/MER_Structured_Datasets_SITE_IM_FY17-19_20190322_v2_1_Coted'Ivoire.rds"
  spectrum_data_source <- "cote/spectrum.xlsx"
}else if(country == "Malawi"){
  mer_data_source <- "malawi/MER_Structured_Datasets_SITE_IM_FY17-19_20190322_v2_1_Malawi.txt"
  spectrum_data_source <- "malawi/spectrum.xlsx"
}else if(country == "Nigeria"){
  #mer_data_source <- "nigeria/MER_Structured_Datasets_SITE_IM_FY17-19_20190322_v2_1_Nigeria.rds"
  mer_data_source <- "nigeria/MER_Structured_Datasets_Site_IM_FY17-19_20190920_v2_1_Nigeria.txt"
  #filename_latlon <- "nigeria/Lat_Long_Nigeria_FacilityReport_2018.rds"
  spectrum_data_source <- "nigeria/spectrum.xlsx"
  world_pop_source <- "nigeria/nga_ppp_2019.tif"
  time_cuts <- c(-0.25, 0)
  frm <- hiv_pos ~ (I(age == "<15") + sitetype + log(worldpop_50 + 1) + pmtct_lin_pred)^2 +  sex + primepartner*age*sex +
    splines::bs(log_hts_tst) +
    splines::bs(time) +
    splines::bs(log_tx) +
    splines::bs(log_plhiv) +
    splines::bs(log_pop_est) +
    splines::bs(pmtct_lin_pred) + 
    I(log_tx^2) + I(log_tx^3) + modality*log_hts_tst + (1  | modality:age ) + 
    (1 | cluster_1 / cluster_2 / cluster_3) + (1 | sitename / modality)
  site_re_formula <- ~(1 | sitename / modality) + (1 | cluster_1 /cluster_2 / cluster_3)
}else if(country == "Lesotho"){
  time_cuts <- c(-0.75,  -0.5, -0.25, 0)
  #mer_data_source <- "lesotho/MER_Structured_Datasets_Site_IM_FY17-19_20190920_v2_1_Lesotho.txt"
  mer_data_source <- "lesotho/Genie_Daily_2ae82f8a6fb84c858d946553580a4caa.txt"
  spectrum_data_source <- "lesotho/spectrum.xlsx"
  world_pop_source <- "lesotho/lso_ppp_2019.tif"
  group_sizes <- c(50,30,15)
  frm <- hiv_pos ~ (age + sitetype) +  sex + primepartner*age*sex +
    log(worldpop_50 + 1) +
    splines::bs(log_hts_tst) +
    splines::bs(time) +
    splines::bs(log_tx) +
    splines::bs(log_plhiv) +
    splines::bs(log_pop_est) +
    splines::bs(pmtct_lin_pred) +  I((modality == "Index") & (time >= 0)) : age +
    (1  | modality ) + ( log_hts_tst - 1 | modality) + (1 | cluster_1 / cluster_3) + 
    (1  | sitename / modality) 
  site_re_formula <- ~(1 | sitename / modality) + (1 | cluster_1 / cluster_3)
  
  index_ratio <- function(ratio, pediatric){
    ratio[is.na(ratio)] <- 0
    #ratio <- 1.5 * ratio
    #ratio[!pediatric & ratio < 1.5]  <- ratio[!pediatric & ratio < 1.5] * 1.25#pmin(1.5, ratio[!pediatric & ratio < 1.5] * 1.4)
    #ratio[pediatric & ratio < .4]  <- ratio[pediatric & ratio < .4] * 1.25#pmin(.4, ratio[pediatric & ratio < .4] * 1.4)
    #ratio[!pediatric & ratio < .5] <- .25
    #ratio[pediatric & ratio < .1] <- .05
    ratio[is.infinite(ratio)] <- 1.5
    ratio
  }
}else if(country == "Tanzania"){
  mer_data_source <- "tanzania/MER_Structured_Dataset_Site_IM_FY17-19_20190621_v2_1_Tanzania.txt"
  spectrum_data_source <- "tanzania/spectrum.xlsx"
  time_cuts <- c(0)
  frm <- hiv_pos ~ (log_plhiv + log_pop_est + log_tx +  
                      log_hts_tst+ age + sitetype)^2 +  sex*snuprioritization + primepartner*age*sex +
    I(log_hts_tst^2) + I(log_hts_tst^3) +
    I(log_plhiv^2) + I(log_plhiv^3) +
    I(log_pop_est^2) + I(log_pop_est^3) + I(modality == "Index") : age +
    I(log_tx^2) + I(log_tx^3) + (1  | modality / age ) + ( log_hts_tst - 1 | modality) + (1 | psnu_t) + 
    (1 + log_hts_tst  | sitename) 
  max_diff <- 1.5
  max_increase <- 1.1
  n_steps <- 6
  total_tests_target <- 1771565*.75
}else if(country == "Kenya"){
  mer_data_source <- "kenya/MER_Structured_Datasets_Site_IM_FY17-19_20190920_v2_1_Kenya.rds"
  spectrum_data_source <- NULL
  frm <- hiv_pos ~ log_hts_tst + age + sitetype + time +  sex + snuprioritization + primepartner + age*sex + 
    (1  | modality / age) + ( log_hts_tst - 1 | modality) + (1 | psnu_t) + 
    (1   | sitename) 
  time_cuts <- c(-0.5, -0.25)
  #frm <- hiv_pos ~ (log_hts_tst+ age + sitetype)^2 +  sex*snuprioritization + primepartner + age*sex +
  #  I(log_hts_tst^2) + I(log_hts_tst^3) +
  #  I(modality == "Index") : age +
  #  (1  | modality / age ) + ( log_hts_tst - 1 | modality) + (1 | psnu_t) + 
  #  (1   | sitename) 
}else if(country == "South Africa"){
  mer_data_source <- "south_africa/MER_Structured_Datasets_Site_IM_FY17-19_20190920_v2_1_South Africa.rds"
  spectrum_data_source <- "south_africa/spectrum.xlsx"
  world_pop_source <- "south_africa/zaf_ppp_2019.tif"
  time_cuts <- c(0)
  frm <- hiv_pos ~ (I(age == "<15") + sitetype + log(worldpop_50 + 1) + pmtct_lin_pred)^2 +  sex + primepartner*age*sex +
    splines::bs(log_hts_tst) +
    splines::bs(time) +
    splines::bs(log_tx) +
    splines::bs(log_plhiv) +
    splines::bs(log_pop_est) +
    splines::bs(pmtct_lin_pred) + 
    I(log_tx^2) + I(log_tx^3) + modality*log_hts_tst + (1  | modality:age ) + 
    (1 | cluster_1 / cluster_2 / cluster_3) + (1 | sitename / modality)
  site_re_formula <- ~(1 | sitename / modality) + (1 | cluster_1 /cluster_2 / cluster_3)
}else{
  warning("unknown country")
}



