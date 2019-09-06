frm <- hiv_pos ~ (log_plhiv + log_pop_est + log_tx + time +  
                    log_hts_tst+ age + sitetype)^2 +  sex*snuprioritization + primepartner*age*sex +
  I(time^2)+ I(time^3) + 
  I(log_hts_tst^2) + I(log_hts_tst^3) +
  I(log_plhiv^2) + I(log_plhiv^3) +
  I(log_pop_est^2) + I(log_pop_est^3) + I((modality == "Index") & (time >= 0)) : age +
  I(log_tx^2) + I(log_tx^3) + modality*log_hts_tst + (1  | modality:age ) + 
  (1 | psnu_t / cluster_1 / cluster_2) + (1 + log_hts_tst| sitename)


glmm_fit <- glmer(frm,
                  family=binomial(), 
                  data=dat_analysis2[in_time,],
                  weights = weight_train[in_time],
                  verbose=verbose,
                  nAGQ = nAGQ)



frm_fixed <- hiv_pos ~ (log_plhiv + log_pop_est + log_tx + time +  
                    log_hts_tst+ age + sitetype)^2 +  sex*snuprioritization + primepartner*age*sex +
  I(time^2)+ I(time^3) + 
  I(log_hts_tst^2) + I(log_hts_tst^3) +
  I(log_plhiv^2) + I(log_plhiv^3) +
  I(log_pop_est^2) + I(log_pop_est^3) + I((modality == "Index") & (time >= 0)) : age +
  I(log_tx^2) + I(log_tx^3) 
frm_rand <- list(
  ~ 1 | psnu_t
)
glmm_fit <- glmmPQL(frm_fixed, 
                    frm_rand, 
                  family=binomial(), 
                  data=dat_analysis2[in_time,],
                  weights = weight_train[in_time])


library(leaflet)
df_new <- dat_analysis %>% filter(time == 0)
df <- df_new %>% 
  group_by(sitename) %>% filter(row_number() == 1) %>%
  merge(latlon %>% select(name,latitude,longitude), by.x = "sitename",by.y="name",all.x=TRUE,all.y=FALSE)
df$sitename_true <- df$sitename
for(nm in c("sitetype","sex","snuprioritization","primepartner","modality")){
  tab <- table(df_new[[nm]])
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


df_new <- dat_analysis %>% 
  filter( time == 0) %>%
  group_by(sitename) %>%
  summarise(observed_yield = 100*sum(hiv_pos * weight) / sum(weight),
            positives=sum(hiv_pos * weight),
            total_tests = sum(weight))
df <- merge(df, df_new, by.x="sitename_true",by.y="sitename", all.x=TRUE, all.y=FALSE)



df$pred <-  predict(glmm$glmm_full_fit, 
          newdata=df, 
          allow.new.levels=TRUE, 
          re.form= ~(1 | psnu_t/cluster_1/cluster_2)) - 
  predict(glmm$glmm_full_fit, 
          newdata=df, 
          allow.new.levels=TRUE, 
          re.form= ~0)
df$pred <- 1 / (1 + exp(-df$pred))
df$rr <- df$pred / median(df$pred)

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








df$rr2 <- rr2 <- pmin(200,round(100*(df$observed_yield/median(df$observed_yield)-1)))
rr3 <- ifelse(rr2 > 0, paste0("+",rr2), as.character(rr2))


pal <- colorNumeric(
  palette = "RdBu",#colorRamp(c("white", "red"), interpolate = "spline"),
  domain = c(-max(abs(rr2)), max(abs(rr2))),
  reverse=TRUE
)


lab <- paste0(
  '
<b>', df$sitename_true,'</b><br><br>
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
            title="Obs. Yield Rate")
m
