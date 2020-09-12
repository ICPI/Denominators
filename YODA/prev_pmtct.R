country <- "Nigeria"
source("R/mer_globals.R")
source("R/mer_extract.R")
source("R/mer_fit.R")
source("R/mer_results.R")
library(leaflet)

library(readstata13)
library(questionr)
library(class)

naiis <- read.dta13("nigeria/NAIIS_190819.dta")
names(naiis) <- tolower(names(naiis))
naiis <- naiis %>% filter(!is.na(bdwght)) %>% filter(age >=15, age <= 49)
prop.table(wtd.table(naiis$final_hiv_status, weights = naiis$bdwght))

mer_data_source <- "nigeria/MER_Structured_Datasets_Site_IM_FY18-20_20200320_v2_1_Nigeria.txt"
dat <- extract_data(mer_data_source, spectrum_data_source)
dat_analysis <- dat$dat_analysis
trans_hts_tst <- dat$trans_hts_tst

latlon <- readRDS("nigeria/Lat_Long_Nigeria_FacilityReport_2018.rds")
names(latlon) <- tolower(names(latlon))
dups <- names(table(latlon$name)[table(latlon$name) != 1])
latlon <- latlon %>% filter(!(name %in% dups))




#dat_analysis2 <- build_spatial_clusters(latlon, dat_analysis)
c0 <- boxed_groups(latlon, dat_analysis, 125)
c1 <- boxed_groups(latlon, dat_analysis, 25)
c2 <- boxed_groups(latlon, dat_analysis, 5)

dat_analysis2 <- dat_analysis
dat_analysis2$cluster_0 <- c0
dat_analysis2$cluster_1 <- c1
dat_analysis2$cluster_2 <- c2
dat_analysis2 <- dat_analysis2 %>% filter(ageasentered != "Unknown Age", 
                                          ageasentered != "+50", 
                                          agecoarse != "<15",
                                          modality == "PMTCT ANC")
in_time <- dat_analysis2$time > -.5

frm <- hiv_pos ~ age + #log_hts_tst +
  (1 | cluster_0 / cluster_1 / cluster_2) + (1 | sitename)


glmm_fit <- glmer(frm,
                  family=binomial(), 
                  data=dat_analysis2[in_time,],
                  weights = dat_analysis2$weight[in_time],
                  verbose=verbose,
                  nAGQ = 0)



# frm_fixed <- hiv_pos ~ (log_plhiv + log_pop_est + log_tx + time +  
#                     log_hts_tst+ age + sitetype)^2 +  sex*snuprioritization + primepartner*age*sex +
#   I(time^2)+ I(time^3) + 
#   I(log_hts_tst^2) + I(log_hts_tst^3) +
#   I(log_plhiv^2) + I(log_plhiv^3) +
#   I(log_pop_est^2) + I(log_pop_est^3) + I((modality == "Index") & (time >= 0)) : age +
#   I(log_tx^2) + I(log_tx^3) 
# frm_rand <- list(
#   ~ 1 | psnu_t
# )
# glmm_fit <- glmmPQL(frm_fixed, 
#                     frm_rand, 
#                   family=binomial(), 
#                   data=dat_analysis2[in_time,],
#                   weights = weight_train[in_time])


df_new <- dat_analysis2 %>% filter(in_time, modality == "PMTCT ANC")
df <- df_new %>% 
  group_by(sitename) %>% filter(row_number() == 1) %>%
  merge(latlon %>% select(name,latitude,longitude), by.x = "sitename",by.y="name",all.x=TRUE,all.y=FALSE)
df$sitename_true <- df$sitename
for(nm in c("sitetype","sex","snuprioritization","primepartner","modality")){
  tab <- tapply(df_new$weight,df_new[[nm]], sum)
  df[[nm]] <- names(tab)[which.max(tab)]
}
df$age <- "30-39"
df$ageasentered <- "30-34"
df_new <- df_new %>% filter(sitetype==df$sitetype[1],
                            age==df$age[1],
                            sex == df$sex[1],
                            snuprioritization==df$snuprioritization[1],
                            modality==df$modality[1],
                            hts_tst != 0)

df$log_hts_tst <- trans_hts_tst(100)# median(df_new$log_hts_tst)
df$hts_tst <- 100#median(df_new$hts_tst)
df$sitename <- "no_sitename"

df_new <- dat_analysis %>% 
  select(psnu_t, sex, age, ageasentered,  log_plhiv, log_tx, log_pop_est) %>% 
  unique() %>%
  filter(age == "30-39", ageasentered=="30-34")
df <- df %>% select(-log_plhiv, -log_tx, -log_pop_est) %>% 
  merge(df_new,all.x = TRUE,all.y=FALSE)


df_new <- dat_analysis2 %>% 
  filter( in_time) %>%
  group_by(sitename) %>%
  summarise(observed_yield = 100*sum(hiv_pos * weight) / sum(weight),
            positives=sum(hiv_pos * weight),
            total_tests = sum(weight),
            pmtct_observed_yield = 100*sum(hiv_pos * weight * (modality == "PMTCT ANC")) / sum(weight * (modality == "PMTCT ANC")),
            pmtct_positives=sum(hiv_pos * weight * (modality == "PMTCT ANC")),
            pmtct_total_tests = sum(weight * (modality == "PMTCT ANC")))
df <- merge(df, df_new, by.x="sitename_true",by.y="sitename", all.x=TRUE, all.y=FALSE)



df$pred <-  predict(glmm_fit, 
                    newdata=df, 
                    allow.new.levels=TRUE, 
                    re.form= ~(1 | cluster_0/cluster_1/cluster_2) ) - 
  predict(glmm_fit, 
          newdata=df, 
          allow.new.levels=TRUE, 
          re.form= ~0)
df$pred <- 1 / (1 + exp(-df$pred))
df$rr <- df$pred / median(df$pred)

df$phiv_nn <- NA
for(i in 1:nrow(df)){
  lat <- df$latitude[i]
  lon <- df$longitude[i]
  dst <- sqrt((naiis$latnum - lat)^2 + (naiis$longnum - lon)^2)
  ind <- order(dst)[1:1000]
  p <- sum((naiis$final_hiv_status[ind]==1) * naiis$bdwght[ind]) / sum(naiis$bdwght[ind])
  df$phiv_nn[i] <- p
}

df$rr2 <- rr2 <- round(100*(df$rr-1))
rr3 <- ifelse(rr2 > 0, paste0("+",rr2), as.character(rr2))


pal <- colorNumeric(
  palette = "RdBu",#colorRamp(c("white", "red"), interpolate = "spline"),
  domain = c(-max(abs(rr2)), max(abs(rr2))),
  reverse=TRUE
)


lab <- paste0(
  '
<b>', df$sitename_true,'</b><br><br>
<b>% Higher Adj. Yield Rate</b><br>
',rr2,'%<br><br>
<b>Observed</b><br>
Yield : ', round(df$observed_yield,1),'%<br>
N Pos : ', df$positives,'<br>
N Tests : ', df$total_tests,'<br>
')
m <- leaflet(df) %>%
  addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
  #  addTiles() %>% 
  addCircles(lng=~longitude, 
             lat = ~latitude, 
             color = ~pal(rr2), 
             opacity = 1, 
             popup=lab) %>%
  addLegend(pal=pal, 
            position = "bottomright",
            values = ~rr2,
            title="% Higher Adj. Yield Rate")
m

cor(df$rr, df$phiv_nn, method = "spearman")


qplot(df$pred, df$phiv_nn, size=I(.1)) + 
  geom_smooth(method = lm, formula = y ~ splines::bs(x, 3)) + 
  scale_x_log10() + scale_y_log10() + 
  ylab("Proportion HIV+ Near Factility (NAIIS)") +
  xlab("Estimated Proportion PMTCT HIV+ (YODA)") +
  theme_bw()
library(readr)
