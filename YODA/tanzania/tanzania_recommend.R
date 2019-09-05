country <- "Tanzania"
source("R/mer_globals.R")
source("R/mer_extract.R")
source("R/mer_fit.R")
source("R/mer_results.R")


filename <- paste0( "results/", stringr::str_replace_all(mer_data_source,"/","_"), "_results",".RData")
load(file=filename)
source("R/mer_results.R")


da <- dat_analysis
da$simpmod <- as.character(da$modality)
da$simpmod[da$simpmod=="IndexMod"] <- "Index"
da <- da[!(da$modality %in% c("Post ANC1","OtherMod","PMTCT ANC")),]

tier <- read_csv("tanzania/tier.csv")[c("sitename","tier")]
da <- merge(da,tier,all.x=T,all.y=F)
da <- da[is.na(da$tier) | da$tier != 4 | da$simpmod == "Index",]

targets <- data.frame(
  modality=rep("simpmod",5),
  v1 = c("OtherPITC","Index","Pediatric","TBClinic","MobileMod"),
  value=c(373379.75, 132040, 10691.25, 11208,165124)
)


targets <- data.frame(
  modality=rep("simpmod",5),
  v1 = c("OtherPITC","Index","Pediatric","TBClinic","MobileMod"),
  value=c(373379.75, 132040, 10691.25, 11208,165124),
  stringsAsFactors = FALSE
)

cur_tot <- (da %>% filter(time==0) %>% summarise(sum(weight)))[1,1]
q1_tot <- sum(targets$value)
q1_mult <- 1
q4_tot <- sum(targets$value)*2/3 + cur_tot*1/3
q4_mult <- q4_tot / q1_tot
q3_tot <- sum(targets$value)*1/3 + cur_tot*2/3
q3_mult <- q3_tot / q1_tot


max_increase <- 1.5

q3_targets <- targets
q3_targets$value <- q3_targets$value*q3_mult
model_allocations <-  generate_allocations(da, predict_full_fit, trans_hts_tst, 
                                           max_diff=1 + 2.5*(cur_tot / q3_tot-1), 
                                           max_increase=max_increase,
                                           n_steps=n_steps,
                                           total_tests_target = sum(q3_targets$value),
                                           subgroup_target=q3_targets,
                                           include_variables="simpmod")
fname <- paste(country, "_q3_allocations.csv")
write.csv(model_allocations$allocations, file = fname, row.names = FALSE)


q4_targets <- targets
q4_targets$value <- q4_targets$value*q4_mult
model_allocations2 <-  generate_allocations(da, predict_full_fit, trans_hts_tst, 
                                           max_diff=1 + 2.5*(cur_tot / q4_tot-1), 
                                           max_increase=max_increase,
                                           n_steps=n_steps,
                                           total_tests_target = sum(q4_targets$value),
                                           subgroup_target=q4_targets,
                                           include_variables="simpmod")
fname <- paste(country, "_q4_allocations.csv")
write.csv(model_allocations2$allocations, file = fname, row.names = FALSE)


q1_targets <- targets
q1_targets$value <- q1_targets$value*q1_mult
model_allocations3 <-  generate_allocations(da, predict_full_fit, trans_hts_tst, 
                                            max_diff=1 + 2.5*(cur_tot / q1_tot-1), 
                                            max_increase=max_increase,
                                            n_steps=n_steps,
                                            total_tests_target = sum(q1_targets$value),
                                            subgroup_target=q1_targets,
                                            include_variables="simpmod")
fname <- paste(country, "_q1_allocations.csv")
write.csv(model_allocations3$allocations, file = fname, row.names = FALSE)


weeks_in_quarter <- 90 / 7
sn <- "Mafinga Hospital - District Hospital"
df <- da
pilot_allocations <- model_allocations3
rmarkdown::render('pilot_site_report.Rmd', params=list(site=sn))
file.copy('pilot_site_report.html',paste0("Tanzania/",sn,"_q1_2020.html"), overwrite=TRUE)


pilot_allocations <- model_allocations2
rmarkdown::render('pilot_site_report.Rmd', params=list(site=sn))
file.copy('pilot_site_report.html',paste0("Tanzania/",sn,"_q4_2019.html"), overwrite=TRUE)

pilot_allocations <- model_allocations
rmarkdown::render('pilot_site_report.Rmd', params=list(site=sn))
file.copy('pilot_site_report.html',paste0("Tanzania/",sn,"_q3_2019.html"), overwrite=TRUE)

