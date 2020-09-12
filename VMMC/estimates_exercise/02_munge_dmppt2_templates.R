## PROJECT:  VMMC estimates data collection
## AUTHOR:   jdavis | USAID
## LICENSE:  MIT
## PURPOSE:  munge hierarchy data from dmppt2 templates
## Date:     2020-07-27

#libraries-----------------------------------------------------------

library(tidyverse)
library(readxl)
library(glamr)

#globals-------------------------------------------------------------

#folder with templates
templates <- "C:/Users/Josh/Documents/data/fy20_q2_v1/vmmc/templates"

#create list of files
lists <- list.files(templates, "*.xlsx", full.names = TRUE)

#function to munge
get_mc <- function(file) {
  
    sheets <- readxl::excel_sheets(file)
    
    if("SNU" %in% sheets) {
  
    readxl::read_excel(file, sheet = "SNU", skip = 1, col_types = "text") %>% 
        rename_all(~tolower(.))
      
    }
  
}

#append all the data
tmp <- purrr::map_dfr(.x = lists, .f = ~get_mc(.x))

#clean up
df <- tmp %>% 
  select(countrycode, provincecode, snucode, country, province, snu, area_id) %>% 
  filter(!str_detect(snu, "Provincial"),
         !str_detect(snu, "National"),
         !str_detect(snu, "Regional"))

#write--------------------------------------------------------------------------------
df %>% write_csv(file.path(path, "dmppt2_templates_hierarchy_v1.csv"))



