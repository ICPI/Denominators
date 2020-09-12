


pmtct_glmm <- function(dat_analysis, 
                       frm=NULL, 
                       re_frm = NULL,
                       nAGQ=0){
  
  if(is.null(frm)){
    frm <- hiv_pos ~ age + #log_hts_tst +
      (1 | cluster_1 /cluster_2 / cluster_3) + (1 | sitename)
  }
  environment(frm) <- new.env()
  
  if(is.null(re_frm)){
    re_frm <- ~(1 | cluster_1/cluster_2/cluster_3) 
  }
  environment(re_frm) <- new.env()
  
  dat_analysis2 <- dat_analysis %>% filter(ageasentered != "Unknown Age", 
                                            ageasentered != "+50", 
                                            agecoarse != "<15",
                                            modality == "PMTCT ANC")

  glmm_fit <- glmer(frm,
                    family=binomial(), 
                    data=dat_analysis2,
                    weights = dat_analysis2$weight,
                    verbose=verbose,
                    nAGQ = 0)
  
  compute_values <- function(df){
    df$age <- dat_analysis2$age[1]
    predict(glmm_fit, 
                   newdata=df, 
                   allow.new.levels=TRUE, 
                   re.form= re_frm ) - 
    predict(glmm_fit, 
            newdata=df, 
            allow.new.levels=TRUE, 
            re.form= ~0)
  }
  list(model=glmm_fit, 
       compute_values=compute_values)
}






