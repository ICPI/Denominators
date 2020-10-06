out_dir <- "output/"
df_analysis <- readRDS(paste0(out_dir, "df_analysis.rds"))
countries <- unique(df_analysis$countryname)

for(i in 13:length(countries)){
  country <- countries[i]
  print(country)
  source("R/index-analysis.R")
}
