country <- "Kenya"
mer_data_source <- "country_data/2020_q4/MER_Structured_Datasets_Site_IM_FY18-21_20201218_v2_1_Kenya.txt"
spectrum_data_source <- "application/countries/lesotho/spectrum.xlsx"
world_pop_source <- "application/countries/kenya/ken_ppp_2020.tif"
results_location <- "application/countries/kenya/results/"
spectrum_data_source <- NULL#"kenya/spectrum.xlsx"
frm <- hiv_pos ~ log_hts_tst + age + sitetype + time +  sex + snuprioritization + primepartner + age*sex + 
  (1  | modality / age) + ( log_hts_tst - 1 | modality) + (1 | psnu_t) + 
  (1   | sitename) 
frm <- hiv_pos ~ (I(age == "<15") + sitetype + log(worldpop_50 + 1) + pmtct_lin_pred)^2 +  sex + primepartner*age*sex +
  splines::bs(log_hts_tst) +
  splines::bs(time) +
  #splines::bs(log_tx) +
  #splines::bs(log_plhiv) +
  #splines::bs(log_pop_est) +
  splines::bs(pmtct_lin_pred) + 
  #I(log_tx^2) + I(log_tx^3) + 
  modality*log_hts_tst + (1  | modality:age ) + 
  (1 | cluster_1 / cluster_2 / cluster_3) + (1 | sitename / modality)
site_re_formula <- ~(1 | sitename / modality) + (1 | cluster_1 /cluster_2 / cluster_3)
time_cuts <- c(-0.5, -0.25)
