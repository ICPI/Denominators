library(tidyverse)

options(knitr.kable.NA = '')


fit_locs <- "output/fits/"
fit_files <- list.files(fit_locs)

file <- paste0(fit_locs,fit_files[1])

l <- lapply(fit_files, function(file){
  print(file)
  li <- readRDS(paste0(fit_locs,file))
  df <- li$df_result
  df
})
df <- bind_rows(l)

sids <- sort(unique(df$site_id))
max(sids)
site_popups <- sapply(sids, function(sid){
  if(sid %% 1000 == 1)
    print(sid)
  tmp <- df %>%
    filter(site_id == sid) %>%
    arrange(time) %>%
    transmute(
      facility = facility,
      fiscal_year = fiscal_year,
      quarter = quarter,
      `# Non-Index Pos.` = hts_tst_pos_non_index,
      `# Index` = hts_tst_tot_index,
      `# Index Pos.` = hts_tst_pos_index,
      `# Index Tsts / # Non-Index Pos.` = hts_tst_tot_index / hts_tst_pos_non_index,
      `# Index Pos. / # Non-Index Pos.` = hts_tst_pos_index / hts_tst_pos_non_index,
      `Smoothed # Index Tsts / # Non-Index Pos.` = fitted_tsts_per_non_pos,
      `Smoothed # Index Pos. / # Non-Index Pos.` = fitted_pos_per_non_pos
    )
  out <- tmp %>% 
    select(-facility) %>%
    knitr::kable(
      digits = 1,
      format="html", 
      caption = paste(tmp$facility[1], "")) %>%
    as.character()
  out
})

df_site_plot <- df %>%
  left_join(
    data.frame(site_id=sids,
               popup_html=site_popups)
  )
df_site_plot <- sf::st_as_sf(df_site_plot, coords = c("longitude", "latitude"))
df_site_plot <- df_site_plot[order(st_coordinates(df_site_plot)[,2]),]
save("df_site_plot", file="shiny_app/data/data.RData")

cntry_psnu <- df_site_plot %>%
  as.data.frame() %>%
  select(countryname, psnu) %>%
  unique()
save("cntry_psnu", file="shiny_app/data/cntry_psnu.RData")

