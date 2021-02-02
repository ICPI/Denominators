#country <- "Malawi"
source("R/mer_globals.R")
source("R/mer_extract.R")
source("R/mer_fit.R")
source("R/mer_results.R")
source("R/datim_geo.R")
source("R/worldpop.R")
source("R/pmtct.R")
try(source(paste0("application/countries/",tolower(country),"/params.R")))

# Load in MER / Spectrum data
dat <- extract_data(mer_data_source, spectrum_data_source)
dat_analysis <- dat$dat_analysis
trans_hts_tst <- dat$trans_hts_tst


# Add site location and worldpop covariates
locations <- datim_get_locations(str_replace_all(country,"_"," "))
da_locations <- site_locations(dat_analysis, locations)

da_locations$cluster_1 <- boxed_groups(da_locations, group_sizes[1])
da_locations$cluster_2 <- boxed_groups(da_locations, group_sizes[2])
da_locations$cluster_3 <- boxed_groups(da_locations, group_sizes[3])

table(da_locations$cluster_1)
table(da_locations$cluster_2)
table(da_locations$cluster_3)
table(da_locations$location_mode)

wp <- raster::raster(world_pop_source)
cluster <- parallel::makeCluster(pmin(20,parallel::detectCores()))
da_locations$worldpop_10 <- world_pop_count_cluster(da_locations, wp, 10, cluster)
da_locations$worldpop_50 <- world_pop_count_cluster(da_locations, wp, 50, cluster)
parallel::stopCluster(cluster)
rm("wp")
rm("cluster")

dat_analysis <- merge(dat_analysis, da_locations, all.x=TRUE)
dat_analysis$cluster_1 <- as.factor(dat_analysis$cluster_1)
dat_analysis$cluster_2 <- as.factor(dat_analysis$cluster_2)
dat_analysis$cluster_3 <- as.factor(dat_analysis$cluster_3)

tmp <- dat_analysis %>% filter(ageasentered != "Unknown Age", 
                                    ageasentered != "+50", 
                                    agecoarse != "<15",
                                    modality == "PMTCT ANC")
pmtct_fit <- pmtct_glmm(tmp[tmp$time %in% sort(unique(tmp$time))[1:2],],
                        pmtct_formula,
                        pmtct_re_formula)
dat_analysis$pmtct_lin_pred <- pmtct_fit$compute_values(dat_analysis)
da_locations$pmtct_lin_pred <- pmtct_fit$compute_values(da_locations)
#qplot(longitude, latitude, color=pmtct_lin_pred, data=da_locations)

dat_analysis2 <- dat_analysis[dat_analysis$age != "Unknown Age" & 
                                dat_analysis$sex != "Unknown Sex" &
                                dat_analysis$modality != "Index" &
                                dat_analysis$modality != "IndexMod",]


split <- split_sample(dat_analysis2, testing_fraction = .1)

glmm <- fit_glmm(dat_analysis2, split, frm=frm, time_cuts=time_cuts)

gbm <- fit_gbm(dat_anlaysis2, split, glmm)

enet <- fit_glmnet(dat_anlaysis2, split, glmm, gbm)

predict_full_fit <- make_predict_full_fit(glmm, gbm, enet)


# Index testing model
site_id_vars <- c("sitename", "psnu_t", "sitetype", 
                  "snuprioritization", "cluster_1", "cluster_2", "cluster_3")
dat_analysis_non_index <- dat_analysis %>% 
  filter(age != "Unknown Age", 
         time == 0,
         !(modality %in% c("Index","IndexMod"))) %>%
  group_by_at(vars(one_of(c(site_id_vars,"hiv_pos", 
                            "worldpop_10", "worldpop_50", "pmtct_lin_pred")))) %>%
  summarise(weight = sum(weight))
#t1 <- dat_analysis_non_index %>% filter(hiv_pos) %>% ungroup() %>% select(-hiv_pos)
#t2 <- dat_analysis_non_index %>% filter(!hiv_pos) %>% ungroup() %>% select( -hiv_pos)

#t3 <- merge(t1,t2, by=setdiff(names(t1),"weight"))
#dat_analysis_non_index <- t3 %>% 
#  mutate(hts_tst_non_index = weight.x + weight.y,
#         hts_tst_pos_non_index = weight.x) %>%
#  select(-weight.x,-weight.y)

dat_analysis_non_index<- dat_analysis_non_index %>% 
  ungroup() %>% 
  pivot_wider(names_from = hiv_pos, 
              values_from=weight,
              values_fill = list(weight = 0)) %>%
  mutate(hts_tst_non_index = `TRUE` + `FALSE`,
         hts_tst_pos_non_index = `TRUE`) %>%
  select(-`TRUE`,-`FALSE`)  

#%>%
#  pivot_wider(names_from = c(hiv_pos),
#              values_from = weight,
#              values_fill = list(weight = 0)) %>%
#  mutate(hts_tst_non_index = `FALSE` + `TRUE`,
#         hts_tst_pos_non_index = `TRUE`) %>%
#  select(-`TRUE`,-`FALSE`)

dat_analysis_index <- dat_analysis %>% 
  filter(age != "Unknown Age", 
         time == 0,
         modality %in% c("Index","IndexMod")) %>%
  mutate(pediatric = agecoarse == "<15") %>%
  group_by_at(vars(one_of(c(site_id_vars,"hiv_pos", "pediatric",
                            "worldpop_10", "worldpop_50", "pmtct_lin_pred")))) %>%
  summarise(weight = sum(weight)) 
dat_analysis_index <- dat_analysis_index %>%
  ungroup() %>%
  pivot_wider(names_from = c(hiv_pos),
              values_from = weight,
              values_fill = list(weight = 0)) %>%
  mutate(hts_tst_index = `FALSE` + `TRUE`,
         hts_tst_pos_index = `TRUE`) %>%
  select(-`TRUE`,-`FALSE`) %>%
  filter(hts_tst_index != 0)
dat_analysis_index <- merge(dat_analysis_index, dat_analysis_non_index, all.x=TRUE)
dat_analysis_index$hts_tst_non_index[is.na(dat_analysis_index$hts_tst_non_index)] <- 0
dat_analysis_index$hts_tst_pos_non_index[is.na(dat_analysis_index$hts_tst_pos_non_index)] <- 0


glmm_index_fit <- glmer(index_formula, #(1 | cluster_1 / cluster_3 / sitename),
                        family=binomial(), 
                        data=dat_analysis_index %>% filter(hts_tst_index > 0),
                        verbose=verbose,
                        nAGQ = 0)
#tmp <- dat_analysis_index
#tmp$lin_pred <- predict(glmm_index_fit)
#glm_index_fit <- glm(cbind(hts_tst_pos_index, hts_tst_index) ~ I(1 / (1 + exp(-lin_pred)) > .3),
#                     offset = lin_pred,
#                     family=binomial(),
#                     data=tmp)
pd <- predict(glmm_index_fit, 
              newdata=dat_analysis_index, 
              allow.new.levels=TRUE)
qplot(1 / (1 + exp(-pd)),  hts_tst_pos_index / hts_tst_index, size=hts_tst_pos_non_index, 
      color=pediatric, data=dat_analysis_index) + geom_smooth() + geom_abline()






model_allocations <-  generate_allocations(dat_analysis %>% filter(time >= allocation_lookback), 
                                           predict_full_fit, trans_hts_tst, 
                                           glmm_index_fit=glmm_index_fit,
                                           max_diff=max_diff, 
                                           max_increase=max_increase,
                                           n_steps=n_steps,
                                           total_tests_target = total_tests_target,
                                           subgroup_fixed = data.frame(
                                             "modality",
                                             "PMTCT ANC",
                                             stringsAsFactors = FALSE),
                                           index_ratio_func = index_ratio
)

#yield_model <- fit_yield_model(dat_analysis, verbose=verbose)


#model_marginals <- generate_marginals(dat_analysis, yield_model$predict_full_fit, param_sitename=yield_model$param_sitename)

#model_allocations <-  generate_allocations(dat_analysis, yield_model$predict_full_fit, trans_hts_tst, max_diff, n_steps,
#                                           subgroup_fixed = rbind(c("modality","Index"),
#                                                                  c("modality","IndexMod"))
#                                           )
if(!dir.exists(results_location)){
  dir.create(results_location)
}
filename <- paste0( results_location, "results",".RData")
save.image(file=filename)

fname <- paste0(results_location, "allocations.csv")
write.csv(model_allocations$allocations, file = fname, row.names = FALSE)
fname2 <- paste0(results_location, "index_allocations.csv")
write.csv(model_allocations$index_allocations, file = fname2, row.names = FALSE)

tmp <- model_allocations$allocations %>% 
  mutate(pediatric = ageasentered %in% c("<05","<10","01-04","01-09","05-09","10-14","01-10","<01","<15")) %>%
  group_by(psnu_t, ageasentered, pediatric, sex, modality) %>% 
  summarise(
    proposed_hts_tst=sum(proposed_hts_tst),
    expected_hts_tst_pos = sum(expected_new_hiv_cases_at_proposed),
    ) %>%
  ungroup() %>%
  pivot_wider(
    names_from = modality, 
    values_from = c(proposed_hts_tst, expected_hts_tst_pos), 
    values_fill = list(proposed_hts_tst=0,expected_hts_tst_pos=0)
    )
tmpin <- model_allocations$index_allocations %>%
  group_by(psnu_t, pediatric) %>% 
  summarise(
    proposed_hts_tst_IndexIndexMod=sum(proposed_hts_tst_index),
    expected_hts_tst_pos_IndexIndexMod = sum(expected_new_hiv_cases_at_proposed),
  ) 
ind_tst_dist <- dat_analysis %>% 
  mutate(pediatric = age == "<15") %>%
  filter(time == 0, modality %in% c("Index","IndexMod")) %>% 
  group_by(psnu_t, ageasentered, pediatric, sex) %>% 
  summarise(nsub=sum(weight) + .0001, 
            nsub_pos=sum(weight*hiv_pos) + .0001) %>%
  group_by(psnu_t, pediatric) %>%
  mutate(nsub = nsub / sum(nsub), 
         nsub_pos = nsub_pos / sum(nsub_pos))
ind_tst_dist <- ind_tst_dist %>%
  merge(tmpin, all.x=TRUE) %>%
  mutate(
    proposed_hts_tst_IndexIndexMod = proposed_hts_tst_IndexIndexMod * nsub,
    expected_hts_tst_pos_IndexIndexMod = expected_hts_tst_pos_IndexIndexMod * nsub_pos
  )
ind_tst_dist$proposed_hts_tst_IndexIndexMod[is.na(ind_tst_dist$proposed_hts_tst_IndexIndexMod)] <- 0
ind_tst_dist$expected_hts_tst_pos_IndexIndexMod[is.na(ind_tst_dist$expected_hts_tst_pos_IndexIndexMod)] <- 0
allocations_by_psnu <- ind_tst_dist %>% select(-nsub,-nsub_pos) %>% merge(tmp,all=TRUE)
for(i in 5:ncol(allocations_by_psnu)){
  allocations_by_psnu[[i]][is.na(allocations_by_psnu[[i]])] <- 0
}

fname3 <- paste0(results_location, "allocations_by_psnu_modality_sex_age.csv")
write.csv(allocations_by_psnu, file = fname3, row.names = FALSE)

# requires Ckmeans.1d.dp and plotly
rmarkdown::render('mer_report.Rmd')
file.copy("mer_report.html",paste0(results_location,"mer_report",".html"), overwrite = TRUE)
