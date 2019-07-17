##......This code downloads Raster files from WorldPop (https://www.worldpop.org/) website for PEPFAR countries.....##

install.packages("devtools")

##..................This code uses a packages from Github - https://github.com/wpgp/wpgpDownloadR...................##
devtools::install_github("wpgp/wpgpDownloadR")
install.packages("RCurl")
install.packages("bitops")

library(bitops)
library(RCurl)
library(wpgpDownloadR)

#countries<-wpgpListCountries()
#write.table(countries, "countries.csv", sep = ",")
#PEPFAR_Countries <- read_excel("PEPFAR_Countries.xlsx")

## ............................. PEPFAR countries list ISO3 ........................................................##
pepfar_ctr_iso <- c("THA", "AGO" ,"LAO" ,"CHN", "BWA", "MMR", "BDI", "KHM", "CMR", "GUY", "BRB", "JAM" ,"SUR", "TTO",
 "GTM", "NIC", "HND", "PAN", "SLV", "KAZ", "KGZ", "TJK" ,"CIV" ,"COD", "DOM", "SWZ", "ETH" ,"GHA",
 "HTI" ,"IND", "IDN" ,"KEN" ,"LSO", "MWI", "MOZ" ,"NAM", "NGA", "PNG", "RWA", "ZAF", "SSD" ,"TZA",
 "UGA", "UKR", "VNM", "ZMB", "ZWE")
  
for (i in 1:length(pepfar_ctr_iso))
{
  print(pepfar_ctr_iso[i])
  c<-pepfar_ctr_iso[i]
  wpgpGetCountryDataset( ISO3 = c, covariate = "ppp_2015", destDir ="C:/DUAT/DenomWS/wp/wp")
}