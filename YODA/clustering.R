
tmp <- dat_analysis2 %>% 
  filter(time == 0) %>%
  group_by(psnu_t, sitename) %>%
  summarise(observed_yield = round(100*sum(hiv_pos * weight) / sum(weight),1),
            positives=sum(hiv_pos * weight),
            total_tests = sum(weight))
tmp2 <- data.frame(psnu_t=rownames(glmm$re$psnu_t),
                   conditional_odds_ratio_psnu = glmm$re$psnu_t[,1])
tmp3 <- data.frame(sitename=rownames(glmm$re$sitename),
                   conditional_odds_ratio_sitename = glmm$re$sitename[,1])
tmp4 <- merge(merge(tmp, tmp2), tmp3) %>% 
  mutate(conditional_odds_ratio_w_psnu = round(exp(conditional_odds_ratio_sitename + conditional_odds_ratio_psnu),2),
         conditional_odds_ratio_sitename = round(exp(conditional_odds_ratio_sitename),2)) %>%
  select(-conditional_odds_ratio_psnu) %>%
  arrange(desc(conditional_odds_ratio_sitename)) 

tmp4 <- tmp4[c("psnu_t", "sitename","conditional_odds_ratio_sitename", "conditional_odds_ratio_w_psnu", "observed_yield", "positives", "total_tests"
)]

View(tmp4 %>% arrange(psnu_t, desc(conditional_odds_ratio_w_psnu)))

psnus <- unique(tmp4$psnu_t)

psnu <- psnus[1]

v <- 0
dfs <- list()
for(psnu in psnus){
  dfs[[psnu]] <- tmp4 %>% filter(psnu_t == psnu) %>% mutate(s1=scale(log(conditional_odds_ratio_w_psnu))*2.5,s2=scale(log(total_tests)))
  k <- min(ceiling(nrow(dfs[[psnu]])/2), 10)
  ds <- dist(dfs[[psnu]][c("s1","s2")])
  hc1 <- hclust(ds, method = "complete" )
  dfs[[psnu]]$sub_grp <- cutree(hc1, k = k) + v
  v <- max(dfs[[psnu]]$sub_grp)
}
df <- do.call(rbind, dfs)
View(df %>% arrange(desc(sub_grp)))


write.csv(df %>% arrange(desc(sub_grp)) %>% select(-s1,-s2),"nigeria_clustering.csv")
