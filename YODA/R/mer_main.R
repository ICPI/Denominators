#country <- "Malawi"
source("R/mer_globals.R")
source("R/mer_extract.R")
source("R/mer_fit.R")
source("R/mer_results.R")
source("R/datim_geo.R")
source("R/worldpop.R")
source("R/pmtct.R")

# Load in MER / Spectrum data
dat <- extract_data(mer_data_source, spectrum_data_source)
dat_analysis <- dat$dat_analysis
trans_hts_tst <- dat$trans_hts_tst


# Add site location and worldpop covariates
locations <- datim_get_locations(country)
da_locations <- site_locations(dat_analysis, locations)

da_locations$cluster_1 <- boxed_groups(da_locations, group_sizes[1])
da_locations$cluster_2 <- boxed_groups(da_locations, group_sizes[2])
da_locations$cluster_3 <- boxed_groups(da_locations, group_sizes[3])

wp <- raster::raster(world_pop_source)
da_locations$worldpop_10 <- world_pop_count(da_locations, wp, 10)
da_locations$worldpop_50 <- world_pop_count(da_locations, wp, 50)

dat_analysis <- merge(dat_analysis, da_locations, all.x=TRUE)

pmtct_fit <- pmtct_glmm(dat_analysis[dat_analysis$time >= -1.5 & dat_analysis$time < -1,])
dat_analysis$pmtct_lin_pred <- pmtct_fit$compute_values(dat_analysis)
da_locations$pmtct_lin_pred <- pmtct_fit$compute_values(da_locations)


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
