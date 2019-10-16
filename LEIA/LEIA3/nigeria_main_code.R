options(max.print=999999); # remove output limit
options(scipen=999);    # remove scientific notation
tbl<-function(x,y=NULL){if(is.null(y)) {table(x,useNA="ifany")} else {table(x,y,useNA="ifany")}}
###Load Libraries####
library(lessR);library(lubridate);library(dplyr);
library(crosstalk);library(ggplot2);library(latticeExtra);
library(sp);library(spatialEco);library(spdep);
library(GISTools);library(rgdal);library(geosphere);library(rgeos);
library(maptools);library(tmap);library(tmaptools);
library(raster);library(rasterVis);
library(leaflet);library(leaflet.minicharts);library(leaflet.esri);
library(htmlwidgets);library(htmltools);
library(shinythemes);library(shiny);library(shinyWidgets);library(shinydashboard);

#****************************************************************#
source("variables.R")
source("spatial_functions.R")

load("./rdata/nigeria_mer.RData")
#****************************************************************#
#****************************************************************#
### 1. Create SNU1 and SNU2 shapefiles ####
#****************************************************************#
#****************************************************************#
source("1_create_snu1_snu2_shapefiles.R")

#****************************************************************#
#****************************************************************#
### 2. Create dataset ######
source("2_create_dataset.R")
#****************************************************************#
#****************************************************************#
#****************************************************************#
#****************************************************************#
### 3. Get sites coordinates ######
#****************************************************************#
#****************************************************************#
fac_gps<-read.csv("fy2019_facility_uniqueid_gps_corrected.csv",stringsAsFactors=F)

orgunituid<-with(facilities,aggregate(facilities[,c("pos_tot","new_tot","cur_tot")],by=list(sitetype2=sitetype2,orgunituid=orgunituid),FUN=sum,na.rm=T))
orgunituid$link_ind<-1

fac_gps19<-merge(fac_gps,orgunituid[,c("orgunituid","pos_tot","new_tot","cur_tot","link_ind")],by="orgunituid",all.x=T,sort=F)

fac_gps19<-fac_gps19[order(fac_gps19$uniqueid),c("uniqueid","sitetype2","orgunituid","sitename","snu1","snu2","psnu","snu1uid","psnuuid","snu2uid","pos_tot","new_tot","cur_tot","lon","lat")]
row.names(fac_gps19)<-NULL

fac_gps<-fac_gps19
orgunituid2<-fac_gps

#****************************************************************#
#****************************************************************#
### 4. Site neighbors ####
#****************************************************************#
#****************************************************************#
w.lonlat<-spwm(uid=fac_gps$uniqueid,lon=fac_gps$lon,lat=fac_gps$lat)	# get spatial neighbors matrix (correcting for sites in wrong snu2)

#****************************************************************#
#****************************************************************#
### 5. assign agents ######
#****************************************************************#
#****************************************************************#
source("5_abm.R")

#****************************************************************#
#****************************************************************#
### 6. combine results ######
#****************************************************************#
#****************************************************************#
pa55<-assigned
pc55<-carryover
ind55<-neigh.index
nu55<-new.unallocated
pu55<-unassigned

#****************************************************************#
#*** neighbors ***#
n<-nrow(fac_gps)
ct<-unlist(lapply(ind55,length))
names(ct)<-NULL
k<-max(unlist(lapply(ind55,length)))
nn55<-as.data.frame(matrix(nrow=k,ncol=n,dimnames=list(c(paste("neigh00",1:9,sep=""),paste("neigh0",10:99,sep=""),paste("neigh",100:k,sep="")),NULL)))
for(i in 1:n)
{
	nn55[[i]][1:length(ind55[[i]])]<-ind55[[i]]
}
nn55<-t(nn55)
nn55<-data.frame(uid=1:n,nct=ct,nn55)
row.names(nn55)<-NULL

#****************************************************************#
#****************************************************************#
### 7. results ######
#****************************************************************#
#****************************************************************#

source("7_results.R")

#****************************************************************#
#****************************************************************#
### 8. create SNU2 results shapefile ######
#****************************************************************#
#****************************************************************#

source("8_snu2_aggregate_results.R")


#****************************************************************#
#****************************************************************#
### 9. create sites results shapefile ######
#****************************************************************#
#****************************************************************#

source("9_facility_results.R")

#****************************************************************#
#****************************************************************#
### 10. Reshape wide to long dataset ######
#****************************************************************#
#****************************************************************#

source("10_snu2_sites wide2long.R")

save.image("vis.RData")


