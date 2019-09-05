

if(!exists("verbose")) verbose <- 1
vcat <- function(...) if(verbose) cat(...)

if(!exists("country")) country <- "Cote d'Ivoire"
if(!exists("max_diff")) max_diff <- 1.2
if(!exists("n_steps")) n_steps <- 4
frm <- NULL
time_cuts <- c(-0.75,  -0.5, -0.25)
max_increase <- Inf
total_tests_target = NULL
if(country == "Cote d'Ivoire"){
  mer_data_source <- "cote/MER_Structured_Datasets_SITE_IM_FY17-19_20190322_v2_1_Coted'Ivoire.rds"
  spectrum_data_source <- "cote/spectrum.xlsx"
}else if(country == "Malawi"){
  mer_data_source <- "malawi/MER_Structured_Datasets_SITE_IM_FY17-19_20190322_v2_1_Malawi.txt"
  spectrum_data_source <- "malawi/spectrum.xlsx"
}else if(country == "Nigeria"){
  #mer_data_source <- "nigeria/MER_Structured_Datasets_SITE_IM_FY17-19_20190322_v2_1_Nigeria.rds"
  mer_data_source <- "nigeria/MER_Structured_Dataset_Site_IM_FY17-19_20190621_v2_1_Nigeria.rds"
  filename_latlon <- "nigeria/Lat_Long_Nigeria_FacilityReport_2018.rds"
  group_sizes <- c(35,8)
  spectrum_data_source <- "nigeria/spectrum.xlsx"
  frm <- hiv_pos ~ (log_plhiv + log_pop_est + log_tx + time +  
                      log_hts_tst+ age + sitetype)^2 +  sex*snuprioritization + primepartner*age*sex +
    I(time^2)+ I(time^3) + 
    I(log_hts_tst^2) + I(log_hts_tst^3) +
    I(log_plhiv^2) + I(log_plhiv^3) +
    I(log_pop_est^2) + I(log_pop_est^3) + I((modality == "Index") & (time >= 0)) : age +
    I(log_tx^2) + I(log_tx^3) + modality*log_hts_tst + (1  | modality:age ) + 
    (1 | psnu_t / cluster_1 / cluster_2) + (1 + log_hts_tst| sitename)
}else if(country == "Lesotho"){
  time_cuts <- c(-0.75,  -0.5, -0.25, 0)
  mer_data_source <- "lesotho/MER_Structured_Dataset_Site_IM_FY17-19_20190621_v2_1_Lesotho.txt"
  spectrum_data_source <- "lesotho/spectrum.xlsx"
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
}else{
  stop("unknown country")
}



