library(INLA)
library(sp)
library(spdep)
library(dplyr)

out_dir <- "pmtct_prev/output/"
df_analysis <- readRDS(paste0(out_dir, "df_analysis.rds"))
sp_cluster_3 <- readRDS(paste0(out_dir, "sp_cluster_3.rds"))

#if(!exists("country")) country <- "Lesotho"
df_analysis %>% 
  filter(
    countryname == country,
  ) %>% 
  group_by(as.factor(time)) %>% 
  summarize(sum(hts_tst_pos),sum(hts_tst_neg)) %>% 
  print(n=1000)

unq <- !duplicated(as.data.frame(sp_cluster_3[c("countryname","cluster_group")]))
sp_cl <- sp_cluster_3[unq,]
sp_cl <- sp_cl[sp_cl$countryname == country,]
sp_cl$cl_id <- 1:nrow(sp_cl)

df_sub <- df_analysis %>% 
  filter(
    countryname == country,
    hts_tst_pos + hts_tst_neg > 0,
    hts_tst_pos >= 0,
    hts_tst_neg >=0
  )
         
df_sub <- sp::merge(sp_cl[c("cl_id","countryname","cluster_group")] ,
                df_sub,
                by.y = c("countryname","cluster_3"),
                by.x=c("countryname","cluster_group"),
                all.x=F, 
                all.y=T, 
                duplicateGeoms = TRUE)
df_sub$hts_tst_tot <- df_sub$hts_tst_neg + df_sub$hts_tst_pos

df_for_inla <- expand.grid(
  cl_id=unique(sp_cl$cl_id), 
  time_ind=unique(df_sub$time_ind)
)
df_for_inla$ageasentered <- "25-29"
df_for_inla$facility_id <- max(df_sub$facility_id) + 1
df_for_inla$hts_tst_pos <- NA
df_for_inla$hts_tst_tot <- 1
df_for_inla$countryname <- country
df_for_inla <- sp::merge(sp_cl["cl_id"] ,
                    df_for_inla,
                    by.y = c("cl_id"),
                    by.x=c("cl_id"),
                    all.x=F, 
                    all.y=T, 
                    duplicateGeoms = TRUE)
df_for_inla <- rbind(
  df_sub[c("cl_id","time_ind","ageasentered",
           "facility_id","hts_tst_pos","hts_tst_tot",
           "countryname")],
  df_for_inla
)


w_sp_cl <- matrix(0L,nrow=nrow(sp_cl),ncol=nrow(sp_cl))
for(i in 1:(nrow(sp_cl)-1)){
  for(j in (i+1):nrow(sp_cl)){
    if(rgeos::gTouches(sp_cl[i,],sp_cl[j,]))
      w_sp_cl[i,j] <- w_sp_cl[j,i] <- 1L
  }
}

#w_df_sub <- nb2mat(adj_list, style = "B", zero.policy=TRUE) 

df_for_inla$TID3 <- df_for_inla$TID2 <- 
  df_for_inla$TID <- as.numeric(as.factor(df_for_inla$time_ind))
df_for_inla$facility_by_time_id <- as.numeric(as.factor(paste(df_for_inla$facility_id,df_for_inla$time_ind)))
if(length(unique(df_for_inla$cl_id)) > 2){
frm <- hts_tst_pos ~ 1 +
  f(ageasentered, model = "iid") + 
  #f(facility_by_time_id, model="iid") + 
  f(facility_id, model = "iid", group=TID,
    control.group = list(model="ar1")) +
  f(TID2, model = "iid") +
  f(cl_id, model = "besag", graph = w_sp_cl,
    group = TID3, control.group = list(model = "ar1"))
}else{
  frm <- hts_tst_pos ~ 1 +
    f(ageasentered, model = "iid") + 
    #f(facility_by_time_id, model="iid") + 
    f(facility_id, model = "iid", group=TID,
      control.group = list(model="ar1")) +
    f(TID2, model = "ar1", group=cl_id,
      control.group = list(model="iid")) 
    #f(cl_id*TID2, model = "iid")  
}
fit <- inla(
  frm,
  family="binomial",
  Ntrials = df_for_inla$hts_tst_tot,
  data=as.data.frame(df_for_inla),
  num.threads = 40,
  blas.num.threads=20,
  control.compute = list(
    dic = TRUE, 
    waic = TRUE, 
    cpo = TRUE),
  control.predictor = list(compute = TRUE),
  control.inla = list(
    strategy = "gaussian",
    int.strategy = "eb"
    ),
  verbose=TRUE
)
print(summary(fit))



library("parallel")
options(mc.cores = 20)

# Transform marginals and compute posterior mean
#marginals: List of `marginals.fitted.values`from inla model
tmarg <- function(marginals) {
  post.means <- mclapply(marginals, function (marg) {
    # Transform post. marginals
    aux <- marg#inla.tmarginal(function(x) x, marg)
    # Compute posterior mean
    inla.emarginal(function(x) x, aux)
  })
  
  return(as.vector(unlist(post.means)))
}

fitted <- tmarg(fit$marginals.fitted.values)
predicted <- 1 / (1 + exp(-fitted[is.na(df_for_inla$hts_tst_pos)]))
df_result <- df_for_inla[is.na(df_for_inla$hts_tst_pos),]
df_result$fitted <- predicted
#df_plot <- df_result[df_result$time_ind == 19,]
library(sp)
# library(leaflet)
# pal <- colorNumeric("Reds", domain=df_result$fitted)
# m <- df_plot %>% 
#   leaflet() %>%
#   addProviderTiles(providers$CartoDB.Positron) %>%
#   addPolygons(
#     fillColor=~pal(fitted),#pal(fitted), 
#     weight=.25, 
#     color="#ffffff",
#     fillOpacity = 1
#   )
# print(m)



df_raw <- as.data.frame(df_for_inla) %>%
  group_by(cl_id,time_ind) %>%
  summarise(
    hts_tst_pos = sum(hts_tst_pos, na.rm=TRUE),
    hts_tst_tot = sum(hts_tst_tot, na.rm=TRUE),
    yield = hts_tst_pos / hts_tst_tot
    )
df_raw <- sp::merge(sp_cl["cl_id"] ,
                    df_raw,
                         by.y = c("cl_id"),
                         by.x=c("cl_id"),
                         all.x=F, 
                         all.y=T, 
                         duplicateGeoms = TRUE)
df_raw$countryname <- df_result$countryname[1]

saveRDS(
  list(
    frm=frm,
    fit=fit,
    df_result=df_result,
    df_raw=df_raw
  ),
  file=paste0(out_dir,"fits/",country,".rds")
)

# df_raw_plot <- df_raw[df_raw$time_ind == 20,]
# 
# pal <- colorNumeric("Reds", domain=df_raw_plot$yield)
# m <- df_raw_plot %>% 
#   leaflet() %>%
#   addProviderTiles(providers$CartoDB.Positron) %>%
#   addPolygons(
#     fillColor=~pal(yield),#pal(fitted), 
#     weight=.25, 
#     color="#ffffff",
#     fillOpacity = 1
#   )
# print(m)
# 
# 
# 
# 
# library(lme4)
# frm <- cbind(hts_tst_pos, hts_tst_neg) ~ ageasentered + #log_hts_tst +
#   (1 | cluster_1_id / cluster_2_id / cluster_3_id ) + (1 | sitename)
# 
# df_sub <- df[df$time_ind <= 17 & df$time_ind > 15 & df$countryname == "Nigeria",]
# glmm_fit <- glmer(frm,
#                   family=binomial(), 
#                   data=df_sub@data,
#                   verbose=T,
#                   nAGQ = 0)
# tmp <-df_sub
# tmp$facility_id <- -1
# tmp$ageasentered <- "25-29"
# tmp$fitted <- predict(glmm_fit, 
#                       newdata=tmp@data, 
#                       allow.new.levels=TRUE,
#                       re.form= ~(1|cluster_1_id)
#                       )
# 
# pal <- colorNumeric("Reds", domain=tmp$fitted)
# m <- tmp[tmp$cluster_3_id == 1037,] %>%
#   leaflet() %>%
#   addProviderTiles(providers$CartoDB.Positron) %>%
#   addPolygons(
#     fillColor=~pal(fitted), 
#     weight=1, 
#     color="#ffffff",
#   ) %>%
#   addCircles(lng=~longitude.y, 
#              lat = ~latitude.y, 
#              color = ~pal(fitted), 
#              opacity = 1)
  
#print(m)

