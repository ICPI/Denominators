
generate_marginals <- function(dat_analysis, predict_full_fit, param_sitename){
  get_marginals <- function(v){
    lev <- unique(dat_analysis[[v]])
    marginal <- rep(NA,length(lev))
    vcat("Constructing marginals for ", v, "with ", length(lev), " levels", "\n")
    for(i in 1:length(lev)){
      vcat(i," ")
      dat_analysis2 <- dat_analysis %>% 
        transmute_all(function(x) if(is.character(x)) as.factor(x) else x) %>%
        filter(time == 0) %>%
        filter(sex != "Unknown Sex" & age != "Unknown Age")
      dat_analysis2[[v]] <- lev[i]
      #mm3 <- sparse.model.matrix(form, dat_analysis2, xlev=xlev)
      #p <- predict(fit_full, newx=mm3, s=lambda_full)
      #p <- 1 / (1 + exp(-p))
      p <- predict_full_fit(dat_analysis2)
      marginal[i] <- sum(p * dat_analysis2$weight) / sum(dat_analysis2$weight)
    }
    names(marginal) <- lev
    result <- data.frame(category=lev, marginal_yield=marginal)
    names(result)[1] <- v
    row.names(result) <- NULL
    
    dat_analysis2 <- dat_analysis %>% 
      transmute_all(function(x) if(is.character(x)) as.factor(x) else x) %>%
      filter(time == 0) %>%
      group_by(!! rlang::sym(v)) %>%
      summarise(observed_yield = sum(hiv_pos * weight) / sum(weight), positives=sum(hiv_pos * weight), total_tests = sum(weight))
    result <- merge(result,dat_analysis2, all.x=TRUE)
    result$observed_yield[is.na(result$observed_yield)] <- NA
    result$positives[is.na(result$positives)] <- 0
    result$total_tests[is.na(result$total_tests)] <- 0
    result <- result[order(-result$marginal_yield),]
  }
  
  
  psnu_marginals <- get_marginals("psnu_t")
  
  site_marginals <- get_marginals("sitename")
  site_marginals <- merge(param_sitename, site_marginals, all=TRUE)
  site_marginals <- site_marginals[order(-site_marginals$marginal_yield),]
  
  

  list(
      site_marginals=site_marginals,
      psnu_marginals=psnu_marginals)
}



generate_allocations <- function(dat_analysis, predict_full_fit, trans_hts_tst, 
                                 max_diff=1.2, max_increase=Inf, n_steps=4, 
                                 total_tests_target=NULL, subgroup_fixed=NULL, subgroup_target=NULL, 
                                 include_variables=c()){
  variables <- c("age","ageasentered", "sitename","psnu_t","sitetype", "modality", "primepartner",
                 "snuprioritization", "sex","log_plhiv", "log_tx", "log_pop_est",include_variables)
  # get all combinations observed during the analysis period
  pdat <- unique(dat_analysis[variables])
  
  # merge in number of tests at time 0
  tmp <- dat_analysis[dat_analysis$time == 0, ] %>% 
    group_by(ageasentered, snuprioritization, sitetype, psnu_t, sitename, sex, modality, primepartner, obs_id_factor) %>%
    select(ageasentered, snuprioritization, sitetype, psnu_t, sitename, 
           hts_tst, sex, modality, primepartner, obs_id_factor) %>%
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
  pdat <- merge(pdat, tmp, all.x=T)
  if("cluster_1" %in% names(dat_analysis)){
    clvars <- names(dat_analysis)[str_starts(names(dat_analysis), "cluster_")]
    df <- unique(dat_analysis[c("sitename",clvars)])
    pdat <- merge(pdat, df, all.x=TRUE, all.y=FALSE)
  }
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
           sitename %in% active_sites) 
  tot <- if(is.null(total_tests_target)) sum(pdat$hts_tst) else total_tests_target
  pdat$obs_id_factor[is.na(pdat$obs_id_factor)] <- "out_of_sample"
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
  
  # Calculate yield amoung those who would be tested if the testing rate was incremented up.
  yield_amoung_changes <- (pnew * up_hts_tst - pdat$preds * pdat$hts_tst) / (up_hts_tst - pdat$hts_tst)
  yield_amoung_changes[up_hts_tst == pdat$hts_tst] <- pnew[up_hts_tst == pdat$hts_tst]
  if(!is.null(subgroup_fixed)){
    yield_amoung_changes[subgroup] <- -Inf
  }
  
  # Iteratively select and increment the record that maximizes yield
  target_tot <- sum(pdat$hts_tst)
  monitor <- 0
  while(target_tot < tot){
    if(!is.null(subgroup_target)){
      #if(monitor == 1) browser()
      for(i in 1:nrow(subgroup_target)){
        if(is.null(pdat[[subgroup_target[i,1]]]))
          stop("Unknown subgroup")
        subgroup1 <- pdat[[subgroup_target[i,1]]] == subgroup_target[i,2]
        if(sum(pdat$hts_tst[subgroup1]) > subgroup_target[i,3])
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
    }else{
      monitor <- monitor + 1
    }
    
    # Update current test count
    if(pdat$step[up_ind] + 1 > 0)
      new_hts_tst <- ceiling((initial_hts_tst[up_ind] + .0001) * step^(pdat$step[up_ind] + 1))
    else
      new_hts_tst <- floor(step^(pdat$step[up_ind] + 1) * initial_hts_tst[up_ind])
    target_tot <- target_tot + new_hts_tst - pdat$hts_tst[up_ind]

    pdat[up_ind,c("hts_tst","preds","step")] <- c(new_hts_tst, pnew[up_ind], pdat$step[up_ind] + 1)
    
    
    # Update yield upon the next test count increment
    if(pdat$step[up_ind] != n_steps && step^(pdat$step[up_ind] + 1) < max_increase){
      pnew[up_ind] <- preds_step[up_ind, pdat$step[up_ind] + 1 + n_steps + 1]
    }else{
      pnew[up_ind] <- -Inf
      yield_amoung_changes[up_ind] <- -Inf
      next
    } 
    if(pdat$step[up_ind] + 1 > 0)
      new_hts_tst <- ceiling((initial_hts_tst[up_ind] + .0001) * step^(pdat$step[up_ind] + 1))
    else
      new_hts_tst <- floor(step^(pdat$step[up_ind] + 1) * initial_hts_tst[up_ind])
    yield_amoung_changes[up_ind] <- (pnew[up_ind] * new_hts_tst - 
                                       pdat$preds[up_ind] * pdat$hts_tst[up_ind]) / (new_hts_tst - pdat$hts_tst[up_ind])
    if(new_hts_tst - pdat$hts_tst[up_ind] < .5)
      yield_amoung_changes[up_ind] <- pnew[up_ind]
  }
  pdat$log_hts_tst <- trans_hts_tst(pdat$hts_tst)
  
  allocations <- pdat
  tmp <- dat_analysis[dat_analysis$time == 0, ] %>% 
    group_by(ageasentered, snuprioritization, sitetype, psnu_t, sitename, sex, modality, primepartner) %>%
    select(ageasentered, snuprioritization, sitetype, psnu_t, sitename, 
           weight, hiv_pos, sex, modality, primepartner) %>%
    summarise(current_hts_tst = sum(weight), new_hiv_cases = sum(weight * hiv_pos))
  allocations <- merge(allocations, tmp, all.x=TRUE) %>% 
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
                                 -log_hts_tst,       -time,              -step, hts_tst )

  allocations <- allocations %>% mutate(
    expected_new_hiv_cases_at_proposed= preds*proposed_hts_tst,
    estimated_yield_at_proposed=preds) %>%
    select(-preds)
  
  if(!is.null(subgroup_fixed)){
    final_yield <- sum(pdat$hts_tst[!subgroup] * pdat$preds[!subgroup]) / sum(pdat$hts_tst[!subgroup])
  }else{
    final_yield <- sum(pdat$hts_tst * pdat$preds) / sum(pdat$hts_tst)
  }
  
  list(
    allocations=allocations,
    total_tests=tot,
    target_total_tests=target_tot,
    initial_yield=initial_yield,
    final_yield=final_yield,
    max_diff=max_diff,
    n_steps=n_steps,
    total_tests_target=total_tests_target,
    subgroup_target=subgroup_target,
    subgroup_fixed=subgroup_fixed
  )
}


