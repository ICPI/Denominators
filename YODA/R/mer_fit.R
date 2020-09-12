library(xgboost)
library(Matrix)
library(WeightedROC)
library(lme4)
library(glmnet)
library(rBayesianOptimization)


split_sample <- function(dat_analysis2, testing_fraction=.1){
  weight_train <- sapply(dat_analysis2$weight, function(w){
    sum(runif(w) < (1 - testing_fraction))
  })
  weight_train[dat_analysis2$time != 0] <- dat_analysis2$weight[dat_analysis2$time != 0]
  
  weight_test <- dat_analysis2$weight - weight_train  
  vcat("In record testing sample of size ", sum(weight_test),"\n")
  
  list(
    weight_train = weight_train,
    weight_test = weight_test
  )
}

fit_glmm <- function(dat_analysis2, split, frm=NULL, nAGQ=0, time_cuts = c(-0.75,  -0.5, -0.25)){
  weight_train <- split$weight_train
  weight_test <- split$weight_test
  
  vcat("Fitting glmm model to training data\n")
  if(is.null(frm)){
    frm <- hiv_pos ~ (log_plhiv + log_pop_est + log_tx + time +  
                        log_hts_tst+ age + sitetype)^2 +  sex*snuprioritization + primepartner*age*sex +
      I(time^2)+ I(time^3) + 
      I(log_hts_tst^2) + I(log_hts_tst^3) +
      I(log_plhiv^2) + I(log_plhiv^3) +
      I(log_pop_est^2) + I(log_pop_est^3) + I((modality == "Index") & (time >= 0)) : age +
      I(log_tx^2) + I(log_tx^3) + (1  | modality / age ) + ( log_hts_tst - 1 | modality) + (1 | psnu_t) + 
      (1 + log_hts_tst  | sitename)  
    
    
    if(length(unique(dat_analysis2$snuprioritization)) == 1){
      frm <- hiv_pos ~ (log_plhiv + log_pop_est + log_tx + time +  
                          log_hts_tst+ age + sitetype)^2 +  sex + primepartner*age*sex +
        I(time^2)+ I(time^3) + 
        I(log_hts_tst^2) + I(log_hts_tst^3) +
        I(log_plhiv^2) + I(log_plhiv^3) +
        I(log_pop_est^2) + I(log_pop_est^3) + I((modality == "Index") & (time >= 0)) : age +
        I(log_tx^2) + I(log_tx^3) + (1  | modality / age ) + ( log_hts_tst - 1 | modality) + (1 | psnu_t) + 
        (1 + log_hts_tst  | sitename)   
    }
  }
  auc_values <- rep(NA,5)
  auc_training_values <- rep(NA,5)
  fits <- list()
  roc_values <- list()
  for(i in 1:length(time_cuts)){
    in_time <- dat_analysis2$time >= time_cuts[i]
    vcat("Training using data within: ", -time_cuts[i], " of current quarter\n")
    tr <- try({
      df <- dat_analysis2[in_time,] %>% as.data.frame()
      df$model_wts <- weight_train[in_time]  
      glmm_fit <- glmer(frm,
                        family=binomial(), 
                        data=df,
                        weights = model_wts,
                        verbose=verbose,
                        nAGQ = nAGQ)
    })
    if(inherits(tr, "try-error"))
      next
    fits[[i]] <- glmm_fit
    p <- predict(glmm_fit, newdata= dat_analysis2[(weight_train != 0) & in_time,], allow.new.levels=TRUE)
    
    roc <- WeightedROC(p, 
                       dat_analysis2$hiv_pos[(weight_train != 0) & in_time]+0, 
                       weight_train[(weight_train != 0) & in_time])
    auc_train <- WeightedAUC(roc)
    auc_training_values[i] <- auc_train
    vcat("AUC on training data: ", auc_train, "\n")
    
    p <- predict(glmm_fit, newdata= dat_analysis2[(weight_train != 0) & (dat_analysis2$time == 0),], allow.new.levels=TRUE)
    roc <- WeightedROC(p, 
                       dat_analysis2$hiv_pos[(weight_train != 0) & (dat_analysis2$time == 0)]+0, 
                       weight_train[(weight_train != 0) & (dat_analysis2$time == 0)])
    auc_train <- WeightedAUC(roc)
    vcat("AUC on training data with time == 0: ", auc_train, "\n")
    
    p <- predict(glmm_fit, newdata= dat_analysis2[weight_test != 0 & in_time,], allow.new.levels=TRUE)
    roc <- WeightedROC(p, dat_analysis2$hiv_pos[weight_test != 0 & in_time]+0, weight_test[weight_test != 0 & in_time])
    auc_test_in <- WeightedAUC(roc)
    auc_values[i] <- auc_test_in
    vcat("AUC in testing data: ", auc_test_in, "\n")
  }
  
  ind <- which.max(auc_values)
  glmm_fit <- fits[[ind]]
  tcut <- time_cuts[ind]
  in_time <- dat_analysis2$time >= tcut
  
  vcat("Fitting model to full dataset\n")
  dat_full_fit <- dat_analysis2[in_time,]
  dat_full_fit$model_wts <- dat_full_fit$weight
  glmm_full_fit <- glmer(frm,
                         family=binomial(), 
                         data=dat_full_fit,
                         weights = model_wts,
                         verbose=verbose,
                         nAGQ = nAGQ)
  re <- lme4::ranef(glmm_full_fit)
  param_sitename <- data.frame(sitename=row.names(re$sitename), param=exp(re$sitename[,1]))
  
  list(
    glmm_fit = glmm_fit,
    glmm_full_fit=glmm_full_fit,
    re=re,
    param_sitename=param_sitename,
    auc_value=auc_values,
    auc_training_value=auc_training_values,
    ind=ind,
    in_time=in_time,
    tcut=tcut
  )
}

fit_glmnet <-function(dat_anlaysis2, split, glmm, gbm){
  weight_train <- split$weight_train
  weight_test <- split$weight_test
  
  glmm_fit <- glmm$glmm_fit
  glmm_full_fit <- glmm$glmm_full_fit
  in_time <- glmm$in_time
  
  vcat("Layering on elastic net\n")
  dat_full_fit <- dat_analysis2[in_time,]
  
  glmm_preds <- predict(glmm_fit, newdata=dat_full_fit, allow.new.levels=TRUE)
  mm_gbm <- sparse.model.matrix(gbm$frm,data=dat_full_fit)
  dmat <- xgb.DMatrix(mm_gbm, 
                      label=dat_full_fit$hiv_pos,
                      weight=weight_train[in_time],
                      base_margin=glmm_preds)
  gbm_preds <- predict(gbm$gbm_fit, newdata=dmat, outputmargin = TRUE)
  
  mm2 <- sparse.model.matrix(~ obs_id_factor - 1, data=dat_full_fit)
  glmnet_fit <- glmnet( 
    mm2, 
    dat_full_fit$hiv_pos, 
    weights=weight_train[in_time],
    offset = gbm_preds,#glmm_preds,
    family="binomial", 
    alpha=0.95,
    dfmax=50000,
    lambda.min.ratio = 0.0001,
    standardize=FALSE
  )
  glmnet_preds <- predict(glmnet_fit, newx=mm2, newoffset = gbm_preds)
  auc_trace <- apply(glmnet_preds[weight_test[in_time]>0,], 2, function(p){
    roc <- WeightedROC(p, dat_analysis2$hiv_pos[in_time & (weight_test>0)]+0, weight_test[in_time & (weight_test>0)])
    WeightedAUC(roc)
  })
  glmnet_ind <- which.max(auc_trace)
  vcat("auc from ", auc_trace[1], " to ", auc_trace[glmnet_ind],"\nauc_trace:\n")
  print(auc_trace)
  
  vcat("Fitting elastic net on full data\n")
  glmm_preds <- predict(glmm_full_fit, newdata=dat_full_fit, allow.new.levels=TRUE)
  gbm_preds <- gbm$predict_gbm_full(dat_full_fit, glmm_preds)
  
  glmnet_fit_full <- glmnet( 
    mm2, 
    dat_full_fit$hiv_pos, 
    weights=dat_full_fit$weight,
    offset = gbm_preds,
    family="binomial", 
    alpha=0.95,
    lambda.min.ratio = 0.001,
    standardize=FALSE,
    lambda=glmnet_fit$lambda
  )
  glmnet_preds <- predict(glmnet_fit_full, s=glmnet_fit_full$lambda[glmnet_ind], newx=mm2, newoffset = gbm_preds)
  tmp <- dat_full_fit
  tmp$pdiff <- as.vector(glmnet_preds - gbm_preds - glmnet_fit_full$a0[glmnet_ind])
  tmp$porg <- 1 / (1 + exp(-glmm_preds))
  tmp$pnew <- 1 / (1 + exp(-glmnet_preds))
  tmp$affected <- abs(tmp$pdiff) > .0000001
  #View(tmp %>% filter(abs(pdiff) > .0000001) %>% arrange(desc(pdiff)))
  vcat("# of individuals with predictions affected by glmnet\n")
  
  tab <- tmp  %>% 
    group_by(quarter, affected) %>% 
    summarise(n_individual=sum(weight), n_record=n()) %>%
    print()
  
  list(
    glmnet_fit = glmnet_fit,
    glmnet_fit_full=glmnet_fit_full,
    glmnet_ind=glmnet_ind,
    lambda = glmnet_fit_full$lambda[glmnet_ind],
    tab=tab,
    auc_trace=auc_trace
  )
}

fit_gbm <-function(dat_anlaysis2, split, glmm, n_iter=20){
  weight_train <- split$weight_train
  weight_test <- split$weight_test
  
  glmm_fit <- glmm$glmm_fit
  glmm_full_fit <- glmm$glmm_full_fit
  in_time <- glmm$in_time
  
  dat_full_fit <- dat_analysis2[in_time,]
  
  glmm_preds <- predict(glmm_fit, newdata=dat_full_fit, allow.new.levels=TRUE)
  mm <- sparse.model.matrix(~ obs_id_factor - 1, data=dat_full_fit)
  dat_full_fit$glmm_preds <- glmm_preds
  
  frm <- "~ -1"
  vars <- c("log_plhiv", "log_pop_est", "log_tx", "time", "log_hts_tst", "age", 
    "sitetype", "sex", "snuprioritization", "primepartner", "age", "sex", 
    "modality", "psnu_t", "sitename", "obs_id_factor", "glmm_preds", "cluster_1", 
    "cluster_2", "cluster_3", "worldpop_50", "worldpop_10", "pmtct_lin_pred")
  for(var in vars){
    if(length(unique(dat_full_fit[[var]])) > 1 &&
       ! all(is.na(dat_full_fit[[var]]))
    ){
      frm <- paste(frm, var, sep="+")
    }
  }
  frm <- as.formula(frm)

  # frm <- ~log_plhiv + log_pop_est + log_tx + time + log_hts_tst + age + 
  #   sitetype + sex + snuprioritization + primepartner + age + sex + 
  #   modality + psnu_t + sitename + obs_id_factor + glmm_preds + cluster_1 + 
  #   cluster_2 + cluster_3 + worldpop_50 + worldpop_10 + pmtct_lin_pred- 1
  # if(length(unique(dat_analysis2$snuprioritization)) == 1){
  #   frm <- ~ log_plhiv + log_pop_est + log_tx + time + log_hts_tst + age + cluster_1 + 
  #     cluster_2 + cluster_3 + worldpop_50 + worldpop_10 + pmtct_lin_pred + 
  #     sitetype + sex + primepartner + age + sex + 
  #     modality + psnu_t + glmm_preds  + sitename + obs_id_factor- 1    
  # }
  # if(all(is.na(dat_full_fit$log_plhiv))){
  #   frm <- ~ time + log_hts_tst + age + cluster_1 + 
  #     cluster_2 + cluster_3 + worldpop_50 + worldpop_10 + pmtct_lin_pred + 
  #     sitetype + sex + primepartner + age + sex + 
  #     modality + psnu_t + glmm_preds  + sitename + obs_id_factor- 1     
  # }
  mm_gbm <- sparse.model.matrix(frm,data=dat_full_fit)
  dmat <- xgb.DMatrix(mm_gbm, 
                     label=dat_full_fit$hiv_pos,
                     weight=weight_train[in_time],
                     base_margin=glmm_preds)

  obj <- function(eta, gamma, nrounds, max_depth, min_child_weight=1){
    params <- list(
      max_depth=max_depth,
      gamma=gamma, 
      eta=eta,
      min_child_weight=min_child_weight,
      objective = "binary:logistic", 
      eval_metric = "auc")
    fit <- xgboost(dmat,  params=params, nrounds=nrounds, weight = weight_train[in_time], verbose=FALSE)
    print(c(params, nrounds))
    p <- predict(fit, newdata=dmat)
    print(summary(p))
    roc <- WeightedROC(p[weight_test[in_time]>0], dat_analysis2$hiv_pos[in_time & (weight_test>0)]+0, weight_test[in_time & (weight_test>0)])
    auc <- WeightedAUC(roc)
    print(auc)   
    list(Score = auc,
         Pred = p)
  }
  opt_res <- BayesianOptimization(obj,
                                  bounds = list(eta=c(0,.5),
                                                gamma=c(0,1),
                                                nrounds=c(5L,100L),
                                                max_depth = c(2L, 10L)),
                                  init_grid_dt = NULL, init_points = 4, n_iter = n_iter,
                                  acq = "ucb", kappa = 2.576, eps = 0.0,
                                  verbose = TRUE)
  
  
  params <- as.list(opt_res$Best_Par)
  params$objective <- "binary:logistic"
  params$eval_metric = "auc"
  gbm_fit <- xgboost(dmat,  params=params, nrounds=params$nrounds, weight = weight_train[in_time], verbose=FALSE)
  p <- predict(gbm_fit, newdata=dmat)
  print(summary(p))
  roc <- WeightedROC(p[weight_test[in_time]>0], dat_analysis2$hiv_pos[in_time & (weight_test>0)]+0, weight_test[in_time & (weight_test>0)])
  auc <- WeightedAUC(roc)
  
  glmm_full_preds <- predict(glmm_full_fit, newdata=dat_full_fit, allow.new.levels=TRUE)
  dat_full_fit$glmm_preds <- glmm_full_preds
  all_va <- names(get_all_vars(frm, dat_full_fit))
  xlevs <- lapply(dat_full_fit[all_va], 
                  function(x){
                    if(is.factor(x)) 
                      levels(x) 
                    else 
                      NULL
                    })
  xlevs <- xlevs[!sapply(xlevs, is.null)]
  xlevs <- lapply(xlevs, function(x) unique(c(x, "__out_of_sample__")))
  
  mm_gbm <- sparse.model.matrix(frm,data=dat_full_fit, xlev = xlevs)
  dmat <- xgb.DMatrix(mm_gbm, 
                      label=dat_full_fit$hiv_pos,
                      weight=dat_full_fit$weight,
                      base_margin=glmm_full_preds)
  gbm_full_fit <- xgboost(dmat,  params=params, nrounds=params$nrounds, verbose=FALSE)
  
  predict_gbm_full <- function(dat, glmm_preds){
    df <- dat
    dat$glmm_preds <- glmm_preds
    mm_gbm <- sparse.model.matrix(frm, data=dat, xlev=xlevs)
    dmat <- xgb.DMatrix(mm_gbm, 
                        base_margin=glmm_preds)
    predict(gbm_full_fit, newdata=dmat, outputmargin = TRUE)
  }

  list(
    gbm_full_fit=gbm_full_fit,
    gbm_fit=gbm_fit,
    auc=auc,
    opt_res=opt_res,
    params=params,
    frm=frm,
    xlevs=xlevs,
    predict_gbm_full=predict_gbm_full,
    dmat=dmat
  )
}

make_predict_full_fit <- function(glmm, gbm, enet){
  predict_full_fit <- function(dat){
    p <- predict(glmm$glmm_full_fit, newdata= dat, allow.new.levels=TRUE)
    p <- gbm$predict_gbm_full(dat, p)
    mm <- sparse.model.matrix(~ obs_id_factor - 1, data=dat)
    glmnet_preds <- as.vector(predict(enet$glmnet_fit_full, 
                                      s=enet$glmnet_fit$lambda[enet$glmnet_ind], 
                                      newx=mm, 
                                      newoffset = p))
    1 / (1 + exp(-glmnet_preds))
  }
}

