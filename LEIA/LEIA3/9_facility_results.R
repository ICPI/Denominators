
#****************************************************************#
#****************************************************************#
### Import shapefiles ####
#****************************************************************#
#****************************************************************#
facid_pt<-SpatialPointsDataFrame(fac_gps[,c("lon","lat")],fac_gps,proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
writeOGR(obj=facid_pt, dsn="./shapefiles/results/facid_fy2019.shp",layer="facid_fy2019",driver="ESRI Shapefile",overwrite=T) # this is in geographical projection

#****************************************************************#
#****************************************************************#
### linkage ####
#****************************************************************#
#****************************************************************#
fac_op19<-output55
fac_op19$pos_tot[which(fac_op19$pos_tot==0)]<-NA
fac_op19$lnk_tot<-fac_op19$ass_tot/fac_op19$pos_tot

fac_op19$tot1.0001<-fac_op19$ass1.0001+fac_op19$una1.0001;fac_op19$tot1.0104<-fac_op19$ass1.0104+fac_op19$una1.0104;
fac_op19$tot1.0509<-fac_op19$ass1.0509+fac_op19$una1.0509;fac_op19$tot1.1014<-fac_op19$ass1.1014+fac_op19$una1.1014;
fac_op19$tot1.1519<-fac_op19$ass1.1519+fac_op19$una1.1519;fac_op19$tot1.2024<-fac_op19$ass1.2024+fac_op19$una1.2024;
fac_op19$tot1.2529<-fac_op19$ass1.2529+fac_op19$una1.2529;fac_op19$tot1.3034<-fac_op19$ass1.3034+fac_op19$una1.3034;
fac_op19$tot1.3539<-fac_op19$ass1.3539+fac_op19$una1.3539;fac_op19$tot1.4044<-fac_op19$ass1.4044+fac_op19$una1.4044;
fac_op19$tot1.4549<-fac_op19$ass1.4549+fac_op19$una1.4549;fac_op19$tot1.5000<-fac_op19$ass1.5000+fac_op19$una1.5000;
fac_op19$tot2.0001<-fac_op19$ass2.0001+fac_op19$una2.0001;fac_op19$tot2.0104<-fac_op19$ass2.0104+fac_op19$una2.0104;
fac_op19$tot2.0509<-fac_op19$ass2.0509+fac_op19$una2.0509;fac_op19$tot2.1014<-fac_op19$ass2.1014+fac_op19$una2.1014;
fac_op19$tot2.1519<-fac_op19$ass2.1519+fac_op19$una2.1519;fac_op19$tot2.2024<-fac_op19$ass2.2024+fac_op19$una2.2024;
fac_op19$tot2.2529<-fac_op19$ass2.2529+fac_op19$una2.2529;fac_op19$tot2.3034<-fac_op19$ass2.3034+fac_op19$una2.3034;
fac_op19$tot2.3539<-fac_op19$ass2.3539+fac_op19$una2.3539;fac_op19$tot2.4044<-fac_op19$ass2.4044+fac_op19$una2.4044;
fac_op19$tot2.4549<-fac_op19$ass2.4549+fac_op19$una2.4549;fac_op19$tot2.5000<-fac_op19$ass2.5000+fac_op19$una2.5000;
fac_op19$tot3.9999<-fac_op19$ass3.9999+fac_op19$una3.9999;

ind<-(1:ncol(fac_op19))*names(fac_op19)%in%c("tot1.0001","tot1.0104","tot1.0509","tot1.1014","tot1.1519","tot1.2024","tot1.2529","tot1.3034","tot1.3539","tot1.4044","tot1.4549","tot1.5000",
  "tot2.0001","tot2.0104","tot2.0509","tot2.1014","tot2.1519","tot2.2024","tot2.2529","tot2.3034","tot2.3539","tot2.4044","tot2.4549","tot2.5000",
  "tot3.9999")
ind<-ind[ind>0]
for(i in ind)
{
	fac_op19[[i]][which(fac_op19[[i]]==0)]<-NA
}

fac_op19$lnk1.0001<-fac_op19$ass1.0001/fac_op19$tot1.0001;fac_op19$lnk1.0104<-fac_op19$ass1.0104/fac_op19$tot1.0104;
fac_op19$lnk1.0509<-fac_op19$ass1.0509/fac_op19$tot1.0509;fac_op19$lnk1.1014<-fac_op19$ass1.1014/fac_op19$tot1.1014;
fac_op19$lnk1.1519<-fac_op19$ass1.1519/fac_op19$tot1.1519;fac_op19$lnk1.2024<-fac_op19$ass1.2024/fac_op19$tot1.2024;
fac_op19$lnk1.2529<-fac_op19$ass1.2529/fac_op19$tot1.2529;fac_op19$lnk1.3034<-fac_op19$ass1.3034/fac_op19$tot1.3034;
fac_op19$lnk1.3539<-fac_op19$ass1.3539/fac_op19$tot1.3539;fac_op19$lnk1.4044<-fac_op19$ass1.4044/fac_op19$tot1.4044;
fac_op19$lnk1.4549<-fac_op19$ass1.4549/fac_op19$tot1.4549;fac_op19$lnk1.5000<-fac_op19$ass1.5000/fac_op19$tot1.5000;
fac_op19$lnk2.0001<-fac_op19$ass2.0001/fac_op19$tot2.0001;fac_op19$lnk2.0104<-fac_op19$ass2.0104/fac_op19$tot2.0104;
fac_op19$lnk2.0509<-fac_op19$ass2.0509/fac_op19$tot2.0509;fac_op19$lnk2.1014<-fac_op19$ass2.1014/fac_op19$tot2.1014;
fac_op19$lnk2.1519<-fac_op19$ass2.1519/fac_op19$tot2.1519;fac_op19$lnk2.2024<-fac_op19$ass2.2024/fac_op19$tot2.2024;
fac_op19$lnk2.2529<-fac_op19$ass2.2529/fac_op19$tot2.2529;fac_op19$lnk2.3034<-fac_op19$ass2.3034/fac_op19$tot2.3034;
fac_op19$lnk2.3539<-fac_op19$ass2.3539/fac_op19$tot2.3539;fac_op19$lnk2.4044<-fac_op19$ass2.4044/fac_op19$tot2.4044;
fac_op19$lnk2.4549<-fac_op19$ass2.4549/fac_op19$tot2.4549;fac_op19$lnk2.5000<-fac_op19$ass2.5000/fac_op19$tot2.5000;
fac_op19$lnk3.9999<-fac_op19$ass3.9999/fac_op19$tot3.9999;

#****************************************************************#
#****************************************************************#
### create site info ####
#****************************************************************#
#****************************************************************#

fac_op19b<-merge(fac_op19,fyq,by="fyq",all.x=T,sort=F)
fac_op19b<-cbind(row=1:nrow(fac_op19b),fac_op19b[,fac_vars])
fac_op19c<-merge(fac_op19b,fac_gps[,c("uniqueid","sitetype2","sitename","snu1","snu2","psnu","snu1uid","psnuuid","snu2uid","orgunituid","lon","lat")],by="uniqueid",all.x=T,sort=F)
fac_op19c<-fac_op19c[order(fac_op19c$row),]
row.names(fac_op19c)<-NULL


#****************************************************************#
#****************************************************************#
### rename testing fields ####
#****************************************************************#
#****************************************************************#
sites_fy19<-fac_op19c[,abm19_site_vars]
names(sites_fy19)[abm19_site_vars%in%pos_lnk]<-paste("lnk_",agesex,sep="")
names(sites_fy19)[abm19_site_vars%in%pos_ass]<-paste("ass_",agesex,sep="")
names(sites_fy19)[abm19_site_vars%in%pos_una]<-paste("una_",agesex,sep="")
names(sites_fy19)[abm19_site_vars%in%pos_tot]<-paste("pos_",agesex,sep="")


#****************************************************************#
#****************************************************************#
### create shapefile ####
#****************************************************************#
#****************************************************************#
sites_fy19_pt<-SpatialPointsDataFrame(sites_fy19[,c("lon","lat")],sites_fy19,proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
writeOGR(obj=sites_fy19_pt, dsn="./shapefiles/results/fy2019_sites_abm55.shp",layer="sites_abm55",driver="ESRI Shapefile",overwrite=T) # this is in geographical projection

