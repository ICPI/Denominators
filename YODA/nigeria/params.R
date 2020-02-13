#mer_data_source <- "nigeria/MER_Structured_Datasets_SITE_IM_FY17-19_20190322_v2_1_Nigeria.rds"
mer_data_source <- "nigeria/MER_Structured_Datasets_Site_IM_FY17-20_20191220_v2_1_Nigeria.rds"
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