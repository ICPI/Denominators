#country <- "Malawi"
source("R/mer_globals.R")
source("R/mer_extract.R")
source("R/mer_fit.R")
source("R/mer_results.R")

dat <- extract_data(mer_data_source, spectrum_data_source)
dat_analysis <- dat$dat_analysis
trans_hts_tst <- dat$trans_hts_tst

if(exists("filename_latlon")){
  has_site_locations <- TRUE
  latlon <- readRDS(filename_latlon)
  names(latlon) <- tolower(names(latlon))
  latlon$psnu <- latlon$level4name 
  dat_analysis <- build_spatial_clusters(latlon, dat_analysis, group_sizes=group_sizes)
}else{
  has_site_locations <- FALSE
}

dat_analysis2 <- dat_analysis[dat_analysis$age != "Unknown Age" & dat_analysis$sex != "Unknown Sex",]


split <- split_sample(dat_analysis2, testing_fraction = .1)

glmm <- fit_glmm(dat_analysis2, split, frm=frm, time_cuts=time_cuts)

gbm <- fit_gbm(dat_anlaysis2, split, glmm)

enet <- fit_glmnet(dat_anlaysis2, split, glmm, gbm)

predict_full_fit <- make_predict_full_fit(glmm, gbm, enet)


model_allocations <-  generate_allocations(dat_analysis, predict_full_fit, trans_hts_tst, 
                                           max_diff=max_diff, 
                                           max_increase=max_increase,
                                           n_steps=n_steps,
                                          total_tests_target = total_tests_target
                                           )

#yield_model <- fit_yield_model(dat_analysis, verbose=verbose)


#model_marginals <- generate_marginals(dat_analysis, yield_model$predict_full_fit, param_sitename=yield_model$param_sitename)

#model_allocations <-  generate_allocations(dat_analysis, yield_model$predict_full_fit, trans_hts_tst, max_diff, n_steps,
#                                           subgroup_fixed = rbind(c("modality","Index"),
#                                                                  c("modality","IndexMod"))
#                                           )

filename <- paste0( "results/", stringr::str_replace_all(mer_data_source,"/","_"), "_results",".RData")
save.image(file=filename)

rmarkdown::render('mer_report.Rmd')
file.copy("mer_report.html",paste0("mer_report_",tolower(country),".html"), overwrite = TRUE)
