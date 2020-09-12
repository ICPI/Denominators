## PROJECT:  VMMC estimates data collection
## AUTHOR:   jdavis | USAID
## LICENSE:  MIT
## PURPOSE:  munge data from naomi hierarchy
## Date:     2020-07-27

#libraries---------------------------------------------------------
#install.packages("sf")
library(sf)
library(tidyverse)

#globals-----------------------------------------------------------

path <- "C:/Users/Josh/Documents/data/fy20_q2_v1/vmmc"

#munge------------------------------------------------------------

df <- read_sf("C:/Users/Josh/Documents/data/fy20_q2_v1/vmmc/ssa_naomi_countries.dbf",
  recursive = TRUE,
  full.names = TRUE)

# write-------------------------------------------------------------
df %>% write_csv(file.path(path, "naomi_hierarchy_vmmc.csv"))


