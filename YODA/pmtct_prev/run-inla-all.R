
out_dir <- "pmtct_prev/output/"
df_analysis <- readRDS(paste0(out_dir, "df_analysis.rds"))
countries <- unique(df_analysis$countryname)

for(i in 1:length(countries)){
  country <- countries[i]
  print(country)
  try(source("pmtct_prev/inlanders-analysis.R"))
}
