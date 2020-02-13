country <- "Ethiopia"
mer_data_source <- "ethiopia/MER_Structured_Datasets_Site_IM_FY17-20_20191220_v2_1_Ethiopia.txt"
world_pop_source <- "ethiopia/eth_ppp_2020.tif"
spectrum_data_source <- NULL
frm <- hiv_pos ~ log_hts_tst + age + sitetype + time +  sex + snuprioritization + primepartner + age*sex + 
  (1  | modality / age) + ( log_hts_tst - 1 | modality) + (1 | psnu_t) + 
  (1   | sitename) 

frm <- hiv_pos ~ (age + sitetype) +  sex + primepartner*age*sex +
  log(worldpop_50 + 1) +
  log_hts_tst +
  splines::bs(time) +
  #splines::bs(log_tx) +
  #splines::bs(log_plhiv) +
  #splines::bs(log_pop_est) +
  splines::bs(pmtct_lin_pred) + 
  (1  | modality ) + ( log_hts_tst - 1 | modality) + (1 | cluster_1 / cluster_2 / cluster_3) + 
  (1  | sitename / modality) 
site_re_formula <- ~(1 | sitename / modality) + (1 | cluster_1 / cluster_2 / cluster_3)

time_cuts <- c(-.75, -0.5, -0.25)

index_ratio <- function(ratio, pediatric){
  ratio <- ratio * 4
  ratio[is.na(ratio)] <- 0
  ratio[ratio == 0] <- 0
  ratio[is.infinite(ratio)] <- 1.5
  ratio
}

total_tests_target <- 597724 / 4
max_diff <- 1.35
