country <- "Nigeria"
source("R/mer_globals.R")
source("R/mer_extract.R")
source("R/mer_fit.R")
source("R/mer_results.R")

pilot_sites <- c("Nongu u Kristu ke Sudan hen Tiv (NKST) Hospital - Mkar", "Wannune General Hospital", 
                 "Federal Medical Center - Makurdi", "Bishop Murray Medical Center", 
                 "Vandeikya General Hospital", "Nyam Ugbeh Clinic", 
                 "Terrebor General Hospital", "Rumuokrushi Model Primary Health Centre", 
                 "Isiokpo General Hospital", "Agbonchia Model Primary Health Centre", 
                 "Obio Cottage Hospital", "Lagos State University Teaching Hospital", 
                 "Alimosho General Hospital", "Akonwonjo Primary Health Center", 
                 "Agboju Primary Health Center", "Mushin General Hospital",
                 "Ojodu Primary Health Care")

site_upth <- "University of Portharcourt Teaching Hospital"

filename <- paste0( "results/", stringr::str_replace_all(mer_data_source,"/","_"), "_results",".RData")
load(filename)

weeks_in_quarter <- 90 / 7
total_target <- round(weeks_in_quarter * (4899-348))#4898.961538)
max_diff <- 2

df <- dat_analysis %>% filter(sitename %in% pilot_sites)

pilot_allocations <-  generate_allocations(df, predict_full_fit, 
                                           trans_hts_tst, max_diff=max_diff, n_steps=n_steps,
                                           total_tests_target = total_target)
for(site in pilot_sites){
  rmarkdown::render('pilot_site_report.Rmd', params=list(site=site))
  file.copy('pilot_site_report.html',paste0("pilot/",site,".html"), overwrite=TRUE)
}




df <- dat_analysis %>% filter(sitename %in% site_upth)
total_target <- round(weeks_in_quarter * 1500)#4898.961538)
max_diff <- 10

pilot_allocations <-  generate_allocations(df, predict_full_fit, 
                                           trans_hts_tst, max_diff=max_diff, n_steps=n_steps,
                                           total_tests_target = total_target)
rmarkdown::render('pilot_site_report.Rmd', params=list(site=site_upth))
file.copy('pilot_site_report.html',paste0("pilot/",site_upth,".html"), overwrite=TRUE)
