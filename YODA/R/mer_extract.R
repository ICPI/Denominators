library(ICPIutilities)
library(dplyr)
library(readxl)
library(tidyverse)
library(scclust)
#read_msd("../MER_Structured_Datasets_SITE_IM_FY17-19_20190322_v2_1_Coted'Ivoire.txt")



extract_data <- function(mer_data_source, spectrum_data_source){
  vcat("Reading MER Data\n")
  if(str_ends(mer_data_source,"txt")){
    dat <- ICPIutilities::read_msd(mer_data_source, remove_txt = FALSE)
  }else{
    dat <- readRDS(mer_data_source)
  }
  vcat("Processing MER Data\n")
  dat <- rename_official(dat)
  
  if(stringr::str_starts(mer_data_source, "nigeria")){
    psnu <- dat$community
    psnu <- sapply(str_split(psnu, " "), function(x) x[1])
    dat$psnu <- psnu
  }
  if("trendsfine" %in% names(dat)){
    dat[["agefine"]] <- dat[["trendsfine"]]
    dat[["trendsfine"]] <- NULL
  }
  if("trendssemifine" %in% names(dat)){
    dat[["agesemifine"]] <- dat[["trendssemifine"]]
    dat[["trendssemifine"]] <- NULL
  }
  if("trendscoarse" %in% names(dat)){
    dat[["agecoarse"]] <- dat[["trendscoarse"]]
    dat[["trendscoarse"]] <- NULL
  }
  
  if("fiscal_year" %in% names(dat)){
    dat2 <- dat %>% 
      select(-targets,-cumulative) %>% 
      rename(q1=qtr1,q2=qtr2,q3=qtr3,q4=qtr4) %>%
      mutate_at(c("q1","q2","q3","q4"), as.numeric) %>%
      group_by_at(vars(-q1,-q2,-q3,-q4)) %>%
      summarise_at(c("q1","q2","q3","q4"),  sum, na.rm=TRUE) %>%
      ungroup() %>%
      pivot_wider(
        names_from=fiscal_year, 
        values_from=c("q1","q2","q3","q4"), 
        names_prefix = "fy"
        )
    names(dat2) <- sapply(names(dat2), function(nm){
      if(str_starts(nm,"q")){
        nm <- paste(rev(strsplit(nm,"_")[[1]]), collapse="")
      }
      nm
    })
    dat <- dat2
  }
  
  fyvars <- names(dat)[substr(names(dat),1,4) == "fy20"]
  for(fy in fyvars){
    if(all(is.na(dat[[fy]]) | dat[[fy]] == 0))
      dat[[fy]] <- NULL
    else
      dat[[fy]] <- as.numeric(dat[[fy]])
  }
  fyvars <- names(dat)[substr(names(dat),1,4) == "fy20"]
  
  count_check <- dat %>% filter(standardizeddisaggregate %in% c("Modality/Age/Sex/Result", "Total Numerator"), 
                              indicator %in% c("HTS_TST","HTS_TST_POS")) %>% group_by(indicator, standardizeddisaggregate) %>% 
    summarise_at(fyvars, function(x) sum(x, na.rm=TRUE))
  
  #
  # get disagged testing data
  #
  dat_sub <- dat[dat$standardizeddisaggregate == "Modality/Age/Sex/Result" & dat$indicator == "HTS_TST",]
  dat_sub <- dat_sub %>% 
    group_by(ageasentered, agesemifine, agecoarse, agefine, sex, sitename, 
             operatingunit, psnu,snuprioritization, sitetype, modality, 
             primepartner,
             facilityuid,psnuuid,communityuid) %>% 
    summarise_at(fyvars, function(x) if(all(is.na(x))) NA else sum(x, na.rm=T)) %>%
    select(-ends_with("_targets")) %>%
    select(-ends_with("apr")) %>%
    pivot_longer(cols=starts_with("fy20"), names_to = "quarter", values_to="hts_tst")
  hts_tst_dat <- dat_sub
  
  dat_sub <- dat[dat$standardizeddisaggregate == "Modality/Age/Sex/Result" & dat$indicator == "HTS_TST_POS",]
  dat_sub <- dat_sub %>% 
    group_by(ageasentered, agesemifine, agecoarse, agefine, sex, sitename, operatingunit, psnu, 
             snuprioritization, sitetype, modality, primepartner,facilityuid,psnuuid,communityuid) %>% 
    summarise_at(fyvars, function(x) if(all(is.na(x))) NA else sum(x, na.rm=T)) %>%
    select(-ends_with("_targets")) %>%
    select(-ends_with("apr")) %>%
    pivot_longer(cols=starts_with("fy20"), names_to = "quarter", values_to="hts_tst_pos")
  hts_tst_pos_dat <- dat_sub
  
  dat_tidy <- merge(hts_tst_pos_dat,hts_tst_dat, all=TRUE)
  dat_tidy$psnu_t <- stringr::str_replace_all(tolower(dat_tidy$psnu),"-"," ")
  dat_tidy <- dat_tidy %>% filter( substr(quarter, 1, 6) %in% c("fy2018","fy2019","fy2020"))
  
  if(!is.null(spectrum_data_source)){
    vcat("Reading Spectrum Data\n")
    spec <- read_excel(spectrum_data_source, guess_max=Inf)
    vcat("Processing Spectrum Data\n")
    spec$variable <- sapply(strsplit(spec$indicatorCode,"\\."), function(x) x[[1]])
    spec$age <- spec$categoryOption_name_1
    spec$sex <- ifelse(spec$categoryOption_name_2 == "Male","Male","Female")
    names(spec) <- tolower(names(spec))
    if(!("psnu2" %in% names(spec))){
      spec$psnu2 <- spec$psnu
    }
    spec$psnu2 <- stringr::str_replace_all(tolower(spec$psnu2),"-"," ")
    
    spec_fine <- spec %>% 
      select(psnu2, age, sex, value, variable) %>% 
      group_by(psnu2, age, sex, variable) %>%
      summarise_all(function(x) sum(x, na.rm=T)) %>%
      spread(variable,value) 
    spec_fine$HIV_PREV <- NULL
    names(spec_fine) <- tolower(names(spec_fine))
    spec_fine_unknown_sex <- spec_fine %>%
      group_by(psnu2, age) %>%
      select(-sex) %>%
      summarise_all(function(x) mean(x, na.rm=T))
    spec_fine_unknown_sex$sex <- "Unknown Sex"
    spec_fine <- spec_fine %>% bind_rows(spec_fine_unknown_sex)
    spec_fine_unknown_age <- spec_fine %>%
      group_by(psnu2, sex) %>%
      select(-age) %>%
      summarise_all(function(x) mean(x, na.rm=T))
    spec_fine_unknown_age$age <- "Unknown Age"
    spec_fine <- spec_fine %>% bind_rows(spec_fine_unknown_age)
    
    spec_fine_unknown_agesex <- spec_fine %>%
      group_by(psnu2) %>%
      select(-age,-sex) %>%
      summarise_all(function(x) mean(x, na.rm=T))
    spec_fine_unknown_agesex$age <- "Unknown Age"
    spec_fine_unknown_agesex$sex <- "Unknown Sex"
    spec_fine <- spec_fine %>% bind_rows(spec_fine_unknown_agesex)
    
    spec_fine_age <- spec_fine %>%
      group_by(psnu2, sex) %>%
      filter(age %in% c("01-04","05-09","1-4","5-9")) %>%
      select(-age) %>%
      summarise_all(function(x) sum(x, na.rm=T))
    spec_fine_age$age <- "1-9"
    spec_fine <- spec_fine %>% bind_rows(spec_fine_age)
    
    spec_fine_age <- spec_fine %>%
      group_by(psnu2, sex) %>%
      filter(age %in% c("<01","01-09","<1","1-9")) %>%
      select(-age) %>%
      summarise_all(function(x) sum(x, na.rm=T))
    spec_fine_age$age <- "<10"
    spec_fine <- spec_fine %>% bind_rows(spec_fine_age)
    
    spec_fine_age <- spec_fine %>%
      group_by(psnu2, sex) %>%
      filter(age %in% c("01-04","<01","1-4","<1")) %>%
      select(-age) %>%
      summarise_all(function(x) sum(x, na.rm=T))
    spec_fine_age$age <- "<05"
    spec_fine <- spec_fine %>% bind_rows(spec_fine_age)
    
    spec_fine_age <- spec_fine %>%
      group_by(psnu2, sex) %>%
      filter(age %in% c("25-29","30-34","35-39","40-44","45-49")) %>%
      select(-age) %>%
      summarise_all(function(x) sum(x, na.rm=T))
    spec_fine_age$age <- "25-49"
    spec_fine <- spec_fine %>% bind_rows(spec_fine_age)
    
    spec_fine_age <- spec_fine %>%
      group_by(psnu2, sex) %>%
      filter(age %in% c("40-44","45-49")) %>%
      select(-age) %>%
      summarise_all(function(x) sum(x, na.rm=T))
    spec_fine_age$age <- "40-49"
    spec_fine <- spec_fine %>% bind_rows(spec_fine_age)
    
    spec_fine$age[spec_fine$age == "1-4"] <- "01-04"
    spec_fine$age[spec_fine$age == "1-9"] <- "01-09"
    spec_fine$age[spec_fine$age == "5-9"] <- "05-09"
    spec_fine$age[spec_fine$age == "<1"] <- "<01"
    
    if(!all(unique(dat_tidy$ageasentered) %in% unique(spec_fine$age))){
      warning("MER age band not captured by spectrum")
    }
    unk_psnu <- !(dat_tidy$psnu_t %in% spec$psnu2)
    if(any(unk_psnu)){
      vcat("PSNUs with no spectrum data (imputing medians):\n")
      nms <- unique(dat_tidy$psnu_t[unk_psnu])
      print(nms)
      
      spec_psnu <- spec_fine %>%
        group_by(sex, age) %>%
        select(-psnu2) %>%
        summarise_all(function(x) median(x, na.rm=T))
      
      for(psnu in nms){
        spec_psnu$psnu2 <- psnu
        spec_fine <- spec_fine %>% bind_rows(spec_psnu)
      }
      
    }
    vcat("Merging Spectrum and MER... ")
    dat_tidy <- merge(
      dat_tidy, 
      spec_fine, 
      by.x=c("psnu_t","ageasentered","sex"),
      by.y=c("psnu2","age","sex"),
      all.x=TRUE,
      all.y=FALSE)
    dat_tidy$perc_lhiv <- dat_tidy$plhiv / dat_tidy$pop_est
    dat_tidy$perc_art <- dat_tidy$tx_curr_subnat / dat_tidy$plhiv
  }else{
    dat_tidy$plhiv <- -1
    dat_tidy$pop_est <- -1
    dat_tidy$tx_curr_subnat <- -1
    dat_tidy$perc_lhiv <- -1
    dat_tidy$perc_art <- -1
  }
  dat_tidy$hts_tst_neg <- dat_tidy$hts_tst - dat_tidy$hts_tst_pos
  dat_tidy$obs_id <- 1:nrow(dat_tidy)
  
  #vcat("PSNUs with no spectrum data:\n")
  #print(unique(dat_tidy[is.na(dat_tidy$plhiv),]$psnu_t))
  
  dat_analysis <- dat_tidy %>% gather("outcome","weight", hts_tst_pos, hts_tst_neg)
  dat_analysis$hiv_pos <- dat_analysis$outcome == "hts_tst_pos"
  dat_analysis$weight[is.na(dat_analysis$weight)] <- 0
  dat_analysis$year <- as.numeric(substr(dat_analysis$quarter,3,6))
  dat_analysis$time <- dat_analysis$year + as.numeric(substr(dat_analysis$quarter,8,8))/4 - .25
  dat_analysis$time <- dat_analysis$time - max(dat_analysis$time)
  dat_analysis <- dat_analysis[!is.na(dat_analysis$hts_tst) & !is.na(dat_analysis$plhiv) & 
                                 (dat_analysis$ageasentered != "<01") & (dat_analysis$weight != 0) &
                                 (dat_analysis$sitetype != "Military"),]
  age <- dat_analysis$ageasentered
  age[age %in% c("<05","01-04","05-09", "10-14","01-09", "<10")] <- "<15"
  age[age %in% c("40-49","40-44","45-49")] <- "40-49"
  age[age %in% c("30-34","35-39")] <- "30-39"
  dat_analysis$age <- age

  if(any(dat_analysis$weight < 0)){
    cat("Warning: Removing negative testing counts\n")
    neg_check <- dat_analysis %>% group_by(quarter, hiv_pos) %>% summarise(total_negative_count=sum(weight[weight < 0])) %>% print(n=10000)
    dat_analysis <-  dat_analysis %>% filter(weight>0, hts_tst > 0)
  }else{
    neg_check <- data.frame(quarter=c(), hiv_pos=c(), total_negative_count=c())
  }
  
  center_hts_tst <- mean(log(dat_analysis$hts_tst + 1), na.rm=TRUE)
  spread_hts_tst <- sd(log(dat_analysis$hts_tst + 1), na.rm=TRUE)
  trans_hts_tst <- function(hts_tst){
    (log(hts_tst + 1) - center_hts_tst) / spread_hts_tst
  }
  
  dat_analysis <- dat_analysis %>% 
    transmute_all(function(x) if(is.character(x)) as.factor(x) else x) %>%
    mutate(log_plhiv = scale(log(plhiv + 1)), 
           log_tx = scale(log(tx_curr_subnat + 1)),  
           log_pop_est = scale(log(pop_est + 1)),
           log_hts_tst = trans_hts_tst(hts_tst)
           )
  dat_analysis$obs_id_factor <- as.factor(dat_analysis$obs_id)
  dat_analysis$obs_id_factor <- factor(dat_analysis$obs_id_factor, 
                                       levels=c(levels(dat_analysis$obs_id_factor), "out_of_sample"))
  vcat("done\n")
  list(dat_analysis=dat_analysis,
       trans_hts_tst=trans_hts_tst,
       count_check=count_check,
       neg_check=neg_check)
}




balanced_clustering <- function(dd, group_sizes=c(35,  10), initial_clustering=NULL){
  initial_clustering <- as.integer(as.factor(initial_clustering)) - 1L
  class(initial_clustering) <- "scclust"
  attr(initial_clustering, "cluster_count") <- as.integer(max(initial_clustering) + 1)
  groups <- list()
  for(i in 1:length(group_sizes)){
    if(i == 1 & is.null(initial_clustering)){
      g <- hierarchical_clustering(dd, group_sizes[i])
    }else if(i == 1){
      g <- hierarchical_clustering(dd, group_sizes[i], existing_clustering = initial_clustering)
    }else{
      g <- hierarchical_clustering(dd, group_sizes[i], existing_clustering = g)
    }
    groups[[i]] <- as.numeric(as.vector(g))
  }
  groups <- as.data.frame(groups)
  names(groups) <- paste0("cluster_",1:length(group_sizes))
  groups
}


build_spatial_clusters <- function(latlon, dat_analysis, group_sizes=c(35,10)){

  latlon_sub <- latlon %>% filter(!is.na(latitude), ! is.na(longitude), name %in% unique(dat_analysis$sitename))
  nsites <- length(unique(latlon_sub$name))
  
  dd <- latlon_sub %>% select(latitude, longitude) %>% as.data.frame() %>% distances()
  psnu <- latlon_sub$level4name 
  clusts <- balanced_clustering(dd, group_sizes, psnu)
  clusts$sitename <- latlon_sub$name
  df <- dat_analysis %>% merge(clusts,all.x=TRUE,all.y=FALSE)
  for(i in 1:length(group_sizes)){
    v <- as.character(df[[paste0("cluster_",i)]])
    v[is.na(v)] <- "unknown_location"
    df[[paste0("cluster_",i)]] <- v
  }
  df
}










