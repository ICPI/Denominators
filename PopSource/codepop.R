# load package
install.packages("sp")
install.packages("rgdal")
install.packages("raster")
install.packages("ggplot2")
library(sp)
library(rgdal)
library(raster)
library(ggplot2)



shape<-readOGR("C:/DUAT/DenomWS/wp/Ethiopia_PROD_6_Woreda_WoredaLsib_2017_Dec/Ethiopia_PROD_6_Woreda_WoredaLsib_2017_Dec/Ethiopia_PROD_6_Woreda_WoredaLsib_2017_Dec.shp",stringsAsFactors=F)    # the shape file
rast<-raster("C:/DUAT/DenomWS/wp/eth_ppp_2018.tif") # LandScan Global
plot(rast)

map <- ggplot() + geom_polygon(data = shape, aes(x = long, y = lat, group = group), colour = "black", fill = NA) 
map + theme_void()



ras_zstats <- extract(rast, shape, fun = 'mean', df = T, na.rm = T)


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