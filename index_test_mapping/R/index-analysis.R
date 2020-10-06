library(INLA)
library(sp)
library(spdep)
library(dplyr)
library(ggplot2)
out_dir <- "output/"
df_analysis <- readRDS(paste0(out_dir, "df_analysis.rds"))
sp_cluster <- readRDS(paste0(out_dir, "sp_cluster_1.rds"))
sp_cluster_name <- "cluster_1"

if(!exists("country")) country <- "Lesotho"
df_analysis %>% 
  filter(
    countryname == country,
  ) %>% 
  group_by(as.factor(time)) %>% 
  summarize(
    pni=sum(hts_tst_pos_non_index),
    pi=sum(hts_tst_pos_index),
    ni=sum(hts_tst_neg_index),
    itr=sum(hts_tst_pos_index+hts_tst_neg_index) / sum(hts_tst_pos_non_index),
    ipr=sum(hts_tst_pos_index) / sum(hts_tst_pos_non_index),
  ) %>% 
  print(n=1000)

unq <- !duplicated(as.data.frame(sp_cluster[c("countryname","cluster_group")]))
sp_cl <- sp_cluster[unq,]
sp_cl <- sp_cl[sp_cl$countryname == country,]
sp_cl$cl_id <- 1:nrow(sp_cl)

df_sub <- df_analysis %>%
  filter(
    countryname == country,
    hts_tst_pos_non_index > 0,
    !(location_mode %in% c("median_psnuuid","psnuuid"))
  )
if(nrow(df_sub) == 0) next

df_sub <- sp::merge(sp_cl[c("cl_id","countryname","cluster_group")] ,
                    df_sub,
                    by.y = c("countryname",sp_cluster_name),
                    by.x=c("countryname","cluster_group"),
                    all.x=F, 
                    all.y=T, 
                    duplicateGeoms = TRUE) 

df_for_inla <- expand.grid(
  cl_id=unique(sp_cl$cl_id), 
  time_ind=unique(df_sub$time_ind)
)
df_for_inla$site_id <- max(df_sub$site_id) + 1
df_for_inla$hts_tst_pos_index <- NA
df_for_inla$hts_tst_neg_index <- NA
df_for_inla$hts_tst_pos_non_index <- 1
df_for_inla$countryname <- country
df_for_inla <- sp::merge(sp_cl["cl_id"] ,
                         df_for_inla,
                         by.y = c("cl_id"),
                         by.x=c("cl_id"),
                         all.x=F, 
                         all.y=T, 
                         duplicateGeoms = TRUE)
df_for_inla <- rbind(
  df_sub[c("cl_id","time_ind",
           "site_id","hts_tst_pos_index","hts_tst_neg_index",
           "hts_tst_pos_non_index",
           "countryname")],
  df_for_inla
)

use_spatial <- nrow(sp_cl) > 1

if(use_spatial){
  w_sp_cl <- matrix(0L,nrow=nrow(sp_cl),ncol=nrow(sp_cl))
  for(i in 1:(nrow(sp_cl)-1)){
    for(j in (i+1):nrow(sp_cl)){
      if(rgeos::gTouches(sp_cl[i,],sp_cl[j,]))
        w_sp_cl[i,j] <- w_sp_cl[j,i] <- 1L
    }
  }
}

#w_df_sub <- nb2mat(adj_list, style = "B", zero.policy=TRUE) 

df_for_inla$TID3 <- df_for_inla$TID2 <- 
  df_for_inla$TID <- as.numeric(as.factor(df_for_inla$time_ind))

if(use_spatial){
frm <- hts_tst_pos_index ~ 1 +
  f(site_id, model = "iid", group=TID,
    control.group = list(model="ar1")) +
  f(TID2, model = "iid") +
  f(cl_id, model = "besag", graph = w_sp_cl,
    group = TID3, control.group = list(model = "ar1"))
} else{
  frm <- hts_tst_pos_index ~ 1 +
    f(site_id, model = "iid", group=TID,
      control.group = list(model="ar1")) +
    f(TID2, model = "iid") 
}
fit <- inla(
  frm,
  family="poisson",
  #Ntrials = df_for_inla$hts_tst_tot,
  data=as.data.frame(df_for_inla),
  offset=log(df_for_inla$hts_tst_pos_non_index),
  num.threads = 40,
  blas.num.threads=20,
  control.compute = list(
    dic = TRUE, 
    waic = TRUE, 
    cpo = TRUE),
  control.predictor = list(compute = TRUE,link=1),
  #control.inla = list(
  #  strategy = "gaussian",
  #  int.strategy = "eb"
  #  ),
  verbose=TRUE
)
print(summary(fit))




predicted <- fit$summary.fitted.values[,1] / df_for_inla$hts_tst_pos_non_index
df_for_inla$fitted_pos_per_non_pos <- predicted

if(use_spatial){
  frm_tsts <- hts_tst_pos_index + hts_tst_neg_index ~ 1 +
    f(site_id, model = "iid", group=TID,
      control.group = list(model="ar1")) +
    f(TID2, model = "iid") +
    f(cl_id, model = "besag", graph = w_sp_cl,
      group = TID3, control.group = list(model = "ar1"))
} else {
  frm_tsts <- hts_tst_pos_index + hts_tst_neg_index ~ 1 +
    f(site_id, model = "iid", group=TID,
      control.group = list(model="ar1")) +
    f(TID2, model = "iid")
}

fit_tsts <- inla(
  frm_tsts,
  family="poisson",
  Ntrials = df_for_inla$hts_tst_tot,
  data=as.data.frame(df_for_inla),
  offset=log(df_for_inla$hts_tst_pos_non_index),
  num.threads = 40,
  blas.num.threads=20,
  control.compute = list(
    dic = TRUE, 
    waic = TRUE, 
    cpo = TRUE),
  control.predictor = list(compute = TRUE,link=1),
  #control.inla = list(
  #  strategy = "gaussian",
  #  int.strategy = "eb"
  #  ),
  verbose=TRUE
)
print(summary(fit_tsts))

predicted <- fit_tsts$summary.fitted.values[,1] / df_for_inla$hts_tst_pos_non_index
df_for_inla$fitted_tsts_per_non_pos <- predicted


library(sp)




df_raw <- as.data.frame(df_for_inla) %>%
  filter(!is.na(hts_tst_pos_index)) %>%
  group_by(cl_id,time_ind) %>%
  summarise(
    hts_tst_pos_non_index = sum(hts_tst_pos_non_index, na.rm=TRUE),
    hts_tst_pos_index = sum(hts_tst_pos_index, na.rm=TRUE),
    hts_tst_neg_index = sum(hts_tst_neg_index, na.rm=TRUE),
    index_pos_per_non_index_pos = hts_tst_pos_index / hts_tst_pos_non_index,
    index_tsts_per_non_index_pos = (hts_tst_neg_index+hts_tst_pos_index) / hts_tst_pos_non_index
  )
df_raw <- sp::merge(sp_cl["cl_id"] ,
                    df_raw,
                    by.y = c("cl_id"),
                    by.x=c("cl_id"),
                    all.x=F, 
                    all.y=T, 
                    duplicateGeoms = TRUE)
df_raw$countryname <- df_for_inla$countryname[1]
tmp <- df_for_inla@data %>%
  filter(is.na(hts_tst_pos_index)) %>%
  select(-hts_tst_pos_non_index, -hts_tst_neg_index, -hts_tst_pos_index,
         -countryname, -TID, -TID2, -TID3)
df_raw <- sp::merge(df_raw,
                    tmp,
            by.y = c("cl_id","time_ind"),
            by.x=c("cl_id","time_ind"),
            all.x=F, 
            all.y=T, 
            duplicateGeoms = TRUE)

tmp <- df_for_inla@data %>%
  filter(!is.na(hts_tst_pos_index)) %>%
  select(time_ind, site_id, fitted_pos_per_non_pos, fitted_tsts_per_non_pos)
df_result <- df_analysis %>%
  filter(countryname == country, hts_tst_pos_non_index > 0) %>%
  left_join(
    tmp
  )

pdf(paste0(out_dir,"diagnostics/",country,"1.pdf"))
p <- qplot(fitted_pos_per_non_pos, hts_tst_pos_index / hts_tst_pos_non_index, data=df_result) + 
  geom_abline(slope=1, color="red") + 
  facet_wrap(~time_ind) + 
  scale_y_log10() + 
  scale_x_log10()
print(p)
dev.off()

pdf(paste0(out_dir,"diagnostics/",country,"2.pdf"))
p <- qplot(fitted_tsts_per_non_pos, hts_tst_tot_index / hts_tst_pos_non_index, data=df_result) + 
  geom_abline(slope=1, color="red") + 
  facet_wrap(~time_ind) + 
  scale_y_log10() + 
  scale_x_log10()
print(p)
dev.off()



saveRDS(
  list(
    frm=frm,
    fit=fit,
    frm_tsts=frm_tsts,
    fit_tsts=fit_tsts,
    df_result=df_result,
    df_raw=df_raw
  ),
  file=paste0(out_dir,"fits/",country,".rds")
)
