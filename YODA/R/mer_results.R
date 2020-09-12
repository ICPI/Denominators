# !diagnostics off


generate_allocations <- function(dat_analysis, predict_full_fit, trans_hts_tst, 
                                 glmm_index_fit,
                                 max_diff=1.2, max_increase=Inf, n_steps=4, 
                                 total_tests_target=NULL, subgroup_fixed=NULL, subgroup_target=NULL, 
                                 include_variables=c(),
                                 index_ratio_func = index_ratio){
  
  index_coef <- summary(glmm_index_fit)$coef[,1]
  index_coef <- index_coef[names(index_coef) == "log((hts_tst_index + 1)/(hts_tst_pos_non_index + 1))"]
  if(length(index_coef) == 0)
    stop("Internal Error: index_coef")
  
  variables <- c("age","ageasentered", "sitename","psnu_t","sitetype", "modality", "primepartner",
                 "snuprioritization", "sex","log_plhiv", "log_tx", "log_pop_est", "cluster_1","cluster_2","cluster_3",
                 "worldpop_50", "facilityuid","worldpop_10","pmtct_lin_pred",include_variables)
  # get all combinations observed during the analysis period
  pdat <- unique(dat_analysis[variables])
  
  # merge in number of tests at time 0
  tmp <- dat_analysis[dat_analysis$time == 0, ] %>% 
    select(ageasentered, snuprioritization, sitetype, psnu_t, sitename, 
           hts_tst, sex, modality, primepartner, obs_id_factor) %>%
    group_by(ageasentered, snuprioritization, sitetype, psnu_t, sitename, sex, modality, primepartner, obs_id_factor) %>%
    summarise_all(function(x) unique(x))
  active_partners <- (dat_analysis %>% filter(time == 0 & 
                                                sex != "Unknown Sex" & age != "Unknown Age"))$primepartner %>%
    unique()
  active_modalities <- (dat_analysis %>% filter(time == 0 & 
                                                sex != "Unknown Sex" & age != "Unknown Age"))$modality %>%
    unique()
  active_ages <- (dat_analysis %>% filter(time == 0 & 
                                                  sex != "Unknown Sex" & age != "Unknown Age"))$ageasentered %>%
    unique()
  active_sites <- (dat_analysis %>% filter(time == 0 & 
                                            sex != "Unknown Sex" & age != "Unknown Age"))$sitename %>%
    unique()
  pdat <- merge(pdat, tmp, all.x=T, sort=FALSE)
  
  active_site_types <- (dat_analysis %>% filter(time == 0 & 
                                             sex != "Unknown Sex" & age != "Unknown Age"))$sitetype %>%
    unique()
  pdat <- merge(pdat, tmp, all.x=T, sort=FALSE)
  
  
  pdat$hts_tst[is.na(pdat$hts_tst)] <- 0
  pdat$log_hts_tst <- trans_hts_tst(pdat$hts_tst)
  pdat$time <- 0
  pdat <- pdat %>% 
    filter(!(hts_tst == 0  & ((ageasentered %in% c("<05","01-09","25-49", "<10")) | 
                                snuprioritization == "0 - Missing"))) %>%
    filter(sex != "Unknown Sex" & age != "Unknown Age", 
           primepartner %in% active_partners,
           modality %in% active_modalities,
           ageasentered %in% active_ages,
           sitename %in% active_sites,
           sitetype %in% active_site_types) 
  tot <- if(is.null(total_tests_target)) sum(pdat$hts_tst) else total_tests_target
  pdat$obs_id_factor[is.na(pdat$obs_id_factor)] <- "out_of_sample"
  pdat$pediatric <- pdat$age == "<15"
  
  
  pdat_index <- pdat %>% filter(modality %in% c("Index","IndexMod"))
  pdat <- pdat %>% filter(!(modality %in% c("Index","IndexMod")))
  
  # Construct index test frame including sites with no index tests
  site_id_vars <- c("sitename", "psnu_t", "facilityuid")# "sitetype", "worldpop_50","worldpop_10","pmtct_lin_pred",
                    #"snuprioritization","cluster_1", "cluster_2", "cluster_3")
  pdat$site_id <- apply(pdat[site_id_vars],1, paste, collapse="_")
  pdat_index$site_id <- apply(pdat_index[site_id_vars],1, paste, collapse="_")
  # pi <- pdat_index[pdat_index$hts_tst > 0, c(site_id_vars, "pediatric")] %>% unique()
  # po <- pdat[site_id_vars] %>% unique()
  # no_ind <- !(apply(po,1,paste, collapse="_") %in% apply(pi,1,paste, collapse="_"))
  # po <- po[no_ind,]
  # po$no_ind <- TRUE
  # po <- merge(pdat,po,all.x=TRUE, sort=FALSE)
  # po <- po[!is.na(po$no_ind) & po$hts_tst > 0,]
  # po$modality <- "Index"
  # po$no_ind <- NULL
  # po$preds <- NULL
  # po$log_hts_tst <- NULL
  # po$obs_id_factor <- "out_of_sample"
  # 
  # po <- po %>%
  #   group_by_at(vars(one_of(variables))) %>%
  #   summarise(hts_tst= sum(hts_tst))
  # po$pediatric <- po$age == "<15"
  # po$hts_tst <- po$hts_tst / 1000000
  # po$obs_id_factor <- "out_of_sample"
  # po$time <- 0
  # pdat_index$preds <- NULL
  # pdat_index$log_hts_tst <- NULL
  # po <- po %>% ungroup() %>% mutate_if(is.factor, as.character)
  # pdat_index <- pdat_index %>% mutate_if(is.factor, as.character) %>% mutate_if(is.matrix, as.numeric)
  # pdat_index <-  pdat_index %>% bind_rows(po)
  # 
  # pdat_index_tot <- pdat_index[c(site_id_vars,"hts_tst","pediatric")] %>% 
  #   group_by_at(vars(one_of(c(site_id_vars,"pediatric")))) %>%
  #   summarise(hts_tst_index=sum(hts_tst))
  # pdat_non_index_pos <- dat_analysis[dat_analysis$time == 0 & 
  #                                      dat_analysis$hiv_pos & 
  #                                      !(dat_analysis$modality %in% c("Index","IndexMod")), 
  #                                    c(site_id_vars,"weight")] %>% 
  #   group_by_at(vars(one_of(site_id_vars))) %>%
  #   summarise(hts_tst_pos_non_index=sum(weight))
  # pdat_index_tot <- merge(pdat_index_tot, pdat_non_index_pos, all=TRUE, sort=FALSE)
  # pdat_index_tot$hts_tst_pos_non_index[is.na(pdat_index_tot$hts_tst_pos_non_index)] <- 0
  # pdat_index_tot$hts_tst_index[is.na(pdat_index_tot$hts_tst_index)] <- 0
  # pdat_index_tot$hts_index_per_non_index <- pdat_index_tot$hts_tst_index / pdat_index_tot$hts_tst_pos_non_index
  # 
  # pdat_index_tot$lin_pred <- predict(glmm_index_fit, newdata=pdat_index_tot, allow.new.levels=TRUE)
  # pdat_index_tot$expected_new_hiv_cases_at_proposed <- pdat_index_tot$hts_tst_index * 1 / (1 + exp(-pdat_index_tot$lin_pred))
  # 
  # pdat_index_tot$site_id <- apply(pdat_index_tot[site_id_vars],1, paste, collapse="_")
  # pdat_index_tot_initial <- pdat_index_tot
  
  pdat$preds <- predict_full_fit(pdat)
  pdat <- pdat[order(-pdat$preds),]
  initial_preds <- pdat$preds
  initial_hts_tst <- pdat$hts_tst
  
  
  
  # Precalculate predicted yield rates at each increment of hts_tst
  step <- exp(log(max_diff) / n_steps)
  tmp <- pdat
  preds_up <- list()
  for(s in 1:n_steps){
    tmp$hts_tst <- ceiling(step^s * (pdat$hts_tst + .0001))
    tmp$log_hts_tst <- trans_hts_tst(tmp$hts_tst)
    preds_up[[s]] <- predict_full_fit(tmp)
  }
  preds_down <- list()
  for(s in 1:n_steps){
    tmp$hts_tst <- floor(step^(-s) * pdat$hts_tst)
    tmp$log_hts_tst <- trans_hts_tst(tmp$hts_tst)
    preds_down[[s]] <- predict_full_fit(tmp)
  }
  
  
  initial_hts_tst <- pdat$hts_tst
  subgroup <- rep(FALSE, nrow(pdat))
  if(!is.null(subgroup_fixed)){
    for(i in 1:nrow(subgroup_fixed)){
      subgroup <- subgroup | pdat[[subgroup_fixed[i,1]]] == subgroup_fixed[i,2]
    }
    initial_yield <- sum(pdat$hts_tst[!subgroup] * pdat$preds[!subgroup]) / sum(pdat$hts_tst[!subgroup])
  }else{
    
    initial_yield <- sum(pdat$hts_tst * pdat$preds) / sum(pdat$hts_tst)
  }
  
  # Set to minimum hts_tst
  pdat$hts_tst <- floor(step^(-n_steps) * initial_hts_tst)
  pdat$hts_tst[subgroup] <- initial_hts_tst[subgroup]
  
  pdat$log_hts_tst <- trans_hts_tst(pdat$hts_tst)
  pdat$preds <- predict_full_fit(pdat)
  pdat$step <- -4
  
  preds_step <- as.matrix(cbind(as.data.frame(preds_down[n_steps:1]), initial_preds, as.data.frame(preds_up)))
  new_step <- pmin(4, pdat$step + 1)
  new_step_ind <- new_step + n_steps + 1
  up_hts_tst <- ifelse(new_step > 0,
    ceiling((initial_hts_tst + .0001) * step^(new_step)),
    floor(step^(new_step) * initial_hts_tst)
  )
  pnew <- rep(NA, length(up_hts_tst))
  for(i in 1:(2*n_steps + 1)){
    pnew[new_step_ind == i] <- preds_step[new_step_ind == i,i]
  }
  
  # for(sid in unique(pdat_index_tot$site_id)){
  #   site_ind <- which(pdat$site_id == sid)
  #   site_hts_tst_pos_non_index <- sum(pdat[site_ind,"hts_tst"] * pdat[site_ind,"preds"])
  #   site_ind <- which(pdat_index_tot$site_id == sid)
  #   pdat_index_tot[site_ind,"hts_tst_pos_non_index"] <- site_hts_tst_pos_non_index
  #   #pdat_index_tot[site_ind,"lin_pred"] <- index_coef * lp_change
  #   #pdat_index_tot[site_ind,"hts_tst_index"] <- pdat_index_tot$hts_index_per_non_index[site_ind] * site_hts_tst_pos_non_index
  # }
  
  # construct index frame
  pdat_index_tot <- pdat[c(site_id_vars,"site_id")] %>% rbind(pdat_index[c(site_id_vars,"site_id")]) %>% unique()#unique(pdat[c(site_id_vars,"site_id")])
  pdat_index_tot_p <- pdat_index_tot
  pdat_index_tot$pediatric <- FALSE
  pdat_index_tot_p$pediatric <- TRUE
  pdat_index_tot <- rbind(pdat_index_tot, pdat_index_tot_p)
  
  tmp <- dat_analysis
  tmp$site_id <- apply(tmp[site_id_vars],1, paste, collapse="_")
  tmp <- tmp %>% 
    group_by(site_id) %>% 
    summarise(
      worldpop_10=worldpop_10[1], 
      worldpop_50=worldpop_50[1], 
      pmtct_lin_pred=pmtct_lin_pred[1],
      cluster_1=cluster_1[1], 
      cluster_2=cluster_2[1], 
      cluster_3=cluster_3[1])
  tmp <- merge(pdat_index_tot, tmp, all.x=TRUE, sort=FALSE)
  pdat_index_tot <- tmp
  
  # add in sites with no non-index tests
  #tmp <- (pdat_index %>% filter(!(site_id %in% pdat$site_id)))[c(site_id_vars,"site_id","pediatric")] %>% unique()
  #pdat_index_tot <- rbind(pdat_index_tot, tmp)
  
  # add in number of non index positives
  tmp <- pdat %>%
    mutate(expected_new_hiv_cases_at_proposed = preds * hts_tst) %>%
    group_by(site_id) %>%
    summarise(hts_tst_pos_non_index = sum(expected_new_hiv_cases_at_proposed))
  tmp <- pdat_index_tot %>%
    merge(tmp, all.x=TRUE, sort = FALSE)
  tmp$hts_tst_pos_non_index[is.na(tmp$hts_tst_pos_non_index)] <- 0
  pdat_index_tot <- tmp
  
  # add initial index counts
  tmp <- pdat_index %>% 
    group_by(site_id, pediatric) %>%
    summarise(initial_hts_tst_index = sum(hts_tst, na.rm=TRUE))
  tmp <- merge(pdat_index_tot, tmp, all.x=TRUE, sort=FALSE)
  tmp$initial_hts_tst_index[is.na(tmp$initial_hts_tst_index)] <- 0
  pdat_index_tot <- tmp  
  
  # add initial non-index positive counts
  tmp <- dat_analysis %>% 
    filter(time == 0 & sex != "Unknown Sex" & age != "Unknown Age", hiv_pos, !(modality %in% c("Index","IndexMod"))) 
  tmp$site_id <- apply(tmp[site_id_vars],1, paste, collapse="_")
  tmp <- tmp %>%  
    group_by(site_id) %>%
    summarise(initial_hts_tst_pos_non_index = sum(weight, na.rm=TRUE))
  tmp <- merge(pdat_index_tot, tmp, all.x=TRUE, sort=FALSE)
  tmp$initial_hts_tst_pos_non_index[is.na(tmp$initial_hts_tst_pos_non_index)] <- 0  
  pdat_index_tot <- tmp 
  
  initial_ratio <- pdat_index_tot$initial_hts_tst_index / pdat_index_tot$initial_hts_tst_pos_non_index
  pdat_index_tot$index_ratio <- index_ratio_func(initial_ratio, pdat_index_tot$pediatric)
  pdat_index_tot$hts_tst_index <- pdat_index_tot$hts_tst_pos_non_index * pdat_index_tot$index_ratio
  pdat_index_tot$lin_pred <- predict(glmm_index_fit, newdata=pdat_index_tot, allow.new.levels=TRUE)
  pdat_index_tot$expected_new_hiv_cases_at_proposed <- pdat_index_tot$hts_tst_index * 1 / (1 + exp(-pdat_index_tot$lin_pred ))

  tmp <- pdat_index_tot %>% 
    group_by(site_id) %>%
    summarise(
      hts_tst_index_yield = if(all(hts_tst_pos_non_index == 0) || all(index_ratio == 0)){
         mean(1 / (1 + exp(-lin_pred)))
        }else{
          sum(index_ratio * hts_tst_pos_non_index * 1 / (1 + exp(-lin_pred))) / 
        sum(index_ratio*hts_tst_pos_non_index)
        },
      #old_hts_tst_per_non_index = sum(hts_index_per_non_index*hts_tst_pos_non_index) / sum(hts_tst_pos_non_index),
      hts_index_per_non_index = sum(index_ratio*hts_tst_pos_non_index)
    )
  pdat <- left_join(pdat, tmp, by="site_id")
  
  # Calculate yield amoung those who would be tested if the testing rate was incremented up.
  tot_ni_diff <- ifelse(up_hts_tst == pdat$hts_tst, 1, up_hts_tst - pdat$hts_tst)
  pos_ni_diff <- pmax(0, ifelse(up_hts_tst == pdat$hts_tst, pnew, pnew * up_hts_tst - pdat$preds * pdat$hts_tst))
  tot_i_diff <- pos_ni_diff * pdat$hts_index_per_non_index
  pos_i_diff <- tot_i_diff * pdat$hts_tst_index_yield
  yield_amoung_changes <- (pos_ni_diff + pos_i_diff) / (tot_ni_diff + tot_i_diff)
  #yield_amoung_changes <- (pnew * up_hts_tst - pdat$preds * pdat$hts_tst) / (up_hts_tst - pdat$hts_tst)
  #yield_amoung_changes[up_hts_tst == pdat$hts_tst] <- pnew[up_hts_tst == pdat$hts_tst]
  if(!is.null(subgroup_fixed)){
    yield_amoung_changes[subgroup] <- -Inf
  }
              
  
  
  
  # Iteratively select and increment the record that maximizes yield
  target_tot <- sum(pdat$hts_tst) + sum(pdat_index_tot$hts_tst_index)
  monitor <- 0
  while(target_tot < tot){
    if(!is.null(subgroup_target)){
      #if(monitor == 1) browser()
      for(i in 1:nrow(subgroup_target)){
        if(is.null(pdat[[subgroup_target[i,1]]]))
          stop("Unknown subgroup")
        subgroup1 <- pdat[[subgroup_target[i,1]]] == subgroup_target[i,2]
        cnt <- sum(pdat$hts_tst[subgroup1])
        subgroup2 <- pdat_index_tot[[subgroup_target[i,1]]] == subgroup_target[i,2]
        cnt <- cnt + sum(pdat_index_tot$hts_tst_index[subgroup2], na.rm = TRUE)
        if(cnt > subgroup_target[i,3])
          yield_amoung_changes[subgroup1] <- -Inf
      }
    }
    if(all(is.infinite(yield_amoung_changes))){
      warning("Unable to reach total target")
      break
    }
    
    up_ind <- which.max(yield_amoung_changes)
    
    if(monitor > 1000){
      cat(up_ind," ", target_tot, " ", 
          initial_hts_tst[up_ind], " ", 
          yield_amoung_changes[up_ind], 
          sum(pdat$hts_tst[!subgroup] * pdat$preds[!subgroup]) / sum(pdat$hts_tst[!subgroup]), " ",
          "\n")
      monitor <- 0
      cat("\n")
    }else{
      #cat(".")
      monitor <- monitor + 1
    }
    
    # Update current test count
    if(pdat$step[up_ind] + 1 > 0){
      new_hts_tst <- ceiling((initial_hts_tst[up_ind] + .0001) * step^(pdat$step[up_ind] + 1))
    }else{
      new_hts_tst <- floor(step^(pdat$step[up_ind] + 1) * initial_hts_tst[up_ind])
    }
    target_tot <- target_tot + new_hts_tst - pdat$hts_tst[up_ind]
    #if(new_hts_tst - pdat$hts_tst[up_ind] > 0)
    #  browser()
    pdat[up_ind,c("hts_tst","preds","step")] <- c(new_hts_tst, pnew[up_ind], pdat$step[up_ind] + 1)
    
    
    # Update yield upon the next test count increment
    if(pdat$step[up_ind] != n_steps && step^(pdat$step[up_ind] + 1) < max_increase){
      pnew[up_ind] <- preds_step[up_ind, pdat$step[up_ind] + 1 + n_steps + 1]
    }else{
      pnew[up_ind] <- -Inf
      yield_amoung_changes[up_ind] <- -Inf
      next
    } 
    if(pdat$step[up_ind] + 1 > 0){
      new_hts_tst <- ceiling((initial_hts_tst[up_ind] + .0001) * step^(pdat$step[up_ind] + 1))
    }else{
      new_hts_tst <- floor(step^(pdat$step[up_ind] + 1) * initial_hts_tst[up_ind])
    }
    
    #Number of new tests and expected number of new positives
    tot_ni_diff <- ifelse(new_hts_tst == pdat$hts_tst[up_ind], 
                          1, 
                          new_hts_tst - pdat$hts_tst[up_ind])
    pos_ni_diff <- ifelse(new_hts_tst == pdat$hts_tst[up_ind], 
                          pnew[up_ind], 
                          pnew[up_ind] * new_hts_tst - pdat$preds[up_ind] * pdat$hts_tst[up_ind])

    
    #yield_amoung_changes[up_ind] <- (pnew[up_ind] * new_hts_tst - 
    #                                   pdat$preds[up_ind] * pdat$hts_tst[up_ind]) / (new_hts_tst - pdat$hts_tst[up_ind])
    #if(new_hts_tst - pdat$hts_tst[up_ind] < .5){
    #  yield_amoung_changes[up_ind] <- pnew[up_ind]
    #}
    # Update index tests
    site_ind_pdat <- which(pdat$site_id == pdat$site_id[up_ind])
    site_ind <- which(pdat_index_tot$site_id == pdat$site_id[up_ind])
    site_hts_tst_pos_non_index <- sum(pdat[site_ind_pdat,"hts_tst"] * pdat[site_ind_pdat,"preds"])
    n_ind_tests <- pdat_index_tot$index_ratio[site_ind] * site_hts_tst_pos_non_index
    lp_change <- log( (n_ind_tests + 1) / (site_hts_tst_pos_non_index + 1) ) - 
      log( (pdat_index_tot$hts_tst_index[site_ind] + 1) / 
                        (pdat_index_tot$hts_tst_pos_non_index[site_ind] + 1) )
    #if(all(lp_change != 0) & !all(pdat_index_tot$index_ratio[site_ind] == 0))
    #  browser()
    pdat_index_tot[site_ind,"hts_tst_pos_non_index"] <- site_hts_tst_pos_non_index
    pdat_index_tot[site_ind,"lin_pred"] <- pdat_index_tot[site_ind,"lin_pred"] + index_coef * lp_change
    target_tot <- target_tot + sum(n_ind_tests - pdat_index_tot$hts_tst_index[site_ind])
    pdat_index_tot[site_ind,"hts_tst_index"] <- n_ind_tests
    exp_cases <- pdat_index_tot$hts_tst_index[site_ind] * 1 / (1 + exp(-pdat_index_tot$lin_pred[site_ind] ))
    pdat_index_tot[site_ind,"expected_new_hiv_cases_at_proposed"] <- exp_cases
    
    # number of added index tests and index positives
    tot_i_diff <- sum(pos_ni_diff * pdat_index_tot[site_ind,]$index_ratio) #pos_ni_diff * pdat$hts_index_per_non_index[up_ind]
    pos_i_diff <- sum(pos_ni_diff * pdat_index_tot[site_ind,]$index_ratio * 1 / (1 + exp(-pdat_index_tot$lin_pred[site_ind] ))) #tot_i_diff * pdat$hts_tst_index_yield[up_ind] #wrong

    yield_amoung_changes[up_ind] <- (pos_ni_diff + pos_i_diff) / (tot_ni_diff + tot_i_diff)
    if(is.na(yield_amoung_changes[up_ind]) || is.infinite(yield_amoung_changes[up_ind]))
      stop("internal error")

  }
  #browser()
  pdat$log_hts_tst <- trans_hts_tst(pdat$hts_tst)
  
  allocations <- pdat
  tmp <- dat_analysis[dat_analysis$time == 0, ]  %>% 
    filter(!(modality %in% c("Index","IndexMod"))) %>% 
    group_by(ageasentered, snuprioritization, sitetype, psnu_t, sitename, sex, modality, primepartner) %>%
    select(ageasentered, snuprioritization, sitetype, psnu_t, sitename, 
           weight, hiv_pos, sex, modality, primepartner) %>%
    summarise(current_hts_tst = sum(weight), new_hiv_cases = sum(weight * hiv_pos))
  allocations <- merge(allocations, tmp, all.x=TRUE, sort=FALSE) %>% 
    arrange(desc(preds)) %>%
    mutate(observed_yield = new_hiv_cases / current_hts_tst)
  
  allocations$new_hiv_cases[is.na(allocations$new_hiv_cases)] <- 0
  allocations$current_hts_tst[is.na(allocations$current_hts_tst)] <- 0
  proposed_hts_tst <- allocations$hts_tst
  allocations$hts_tst <- allocations$current_hts_tst
  allocations$log_hts_tst <- trans_hts_tst(allocations$hts_tst)
  allocations$estimated_yield <- predict_full_fit(allocations)
  allocations$proposed_hts_tst <-proposed_hts_tst
  allocations <- allocations %>% select(-age,               -log_plhiv,         -log_tx,            -log_pop_est,      
                                 -log_hts_tst,       -time,              -step, hts_tst, -site_id, -pediatric,
                                 -worldpop_50, -worldpop_10, -pmtct_lin_pred, -obs_id_factor)

  allocations <- allocations %>% mutate(
    expected_new_hiv_cases_at_proposed= preds*proposed_hts_tst,
    estimated_yield_at_proposed=preds) %>%
    select(-preds)
  
  if(!is.null(subgroup_fixed)){
    final_yield <- sum(pdat$hts_tst[!subgroup] * pdat$preds[!subgroup]) / sum(pdat$hts_tst[!subgroup])
  }else{
    final_yield <- sum(pdat$hts_tst * pdat$preds) / sum(pdat$hts_tst)
  }
  
  tmp <- dat_analysis[dat_analysis$time == 0, ]  %>% 
    filter((modality %in% c("Index","IndexMod"))) %>%
    mutate(pediatric = age == "<15")%>% 
    group_by_at(vars(one_of(c(site_id_vars, "pediatric"))))  %>%
    summarise(current_hts_tst_index = sum(weight), 
              current_hts_tst_pos_index = sum(weight * hiv_pos)
              )
  tmp <- tmp %>% 
    ungroup() %>%
    mutate(observed_yield = current_hts_tst_pos_index / current_hts_tst_index)
  tmp <- tmp[c(site_id_vars,
               "pediatric",
               "current_hts_tst_index",
               "current_hts_tst_pos_index",
               "observed_yield"
  )]
  #%>%
  #  select(vars(one_of(c(site_id_vars,
  #                       "pediatric",
  #                       "current_hts_tst_index",
  #                       "current_hts_tst_pos_index",
  #                       "observed_yield"
  #                       )))) %>%
  #  select(site_id, pediatric, current_hts_tst_index, current_hts_tst_pos_index, observed_yield)
  
  
  index_allocations <- pdat_index_tot %>%
    mutate(
      proposed_hts_tst_index = hts_tst_index,
      current_hts_index_per_non_index = initial_hts_tst_index / initial_hts_tst_pos_non_index,
      age = ifelse(pediatric, "<15",">=15"),
      proposed_hts_index_per_non_index = index_ratio,
      expected_hts_tst_pos_non_index = hts_tst_pos_non_index,
      estimated_yield_at_proposed = 1 / (1 + exp(-lin_pred))
      ) %>%
    merge(tmp , by=c(site_id_vars,"pediatric"), all.x=TRUE, sort=FALSE) %>%
    select(-worldpop_10, -worldpop_50, -pmtct_lin_pred, -hts_tst_index,
           -lin_pred, -index_ratio, -hts_tst_pos_non_index,
           -cluster_1, -cluster_2, -cluster_3, -site_id, -initial_hts_tst_index,-initial_hts_tst_pos_non_index) 
  
  list(
    allocations=allocations,
    index_allocations=index_allocations,
    total_tests=tot,
    target_total_tests=target_tot,
    initial_yield_non_index=initial_yield,
    final_yield=final_yield,
    max_diff=max_diff,
    n_steps=n_steps,
    total_tests_target=total_tests_target,
    subgroup_target=subgroup_target,
    subgroup_fixed=subgroup_fixed
  )
}


