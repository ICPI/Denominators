#****************************************************************#
#****************************************************************#
### 	SNU2: wide2long ####
#****************************************************************#
#****************************************************************#
varing_var<-c("ass100","lnk100","pos100","una100","ass101","lnk101","pos101","una101","ass102","lnk102","pos102","una102","ass103","lnk103","pos103","una103",
		    "ass104","lnk104","pos104","una104","ass105","lnk105","pos105","una105","ass106","lnk106","pos106","una106","ass107","lnk107","pos107","una107",
		    "ass108","lnk108","pos108","una108","ass109","lnk109","pos109","una109","ass110","lnk110","pos110","una110","ass111","lnk111","pos111","una111",
		    "ass112","lnk112","pos112","una112","ass113","lnk113","pos113","una113","ass114","lnk114","pos114","una114","ass115","lnk115","pos115","una115",
		    "ass116","lnk116","pos116","una116","ass117","lnk117","pos117","una117","ass118","lnk118","pos118","una118","ass119","lnk119","pos119","una119",
		    "ass120","lnk120","pos120","una120","ass121","lnk121","pos121","una121","ass122","lnk122","pos122","una122","ass123","lnk123","pos123","una123",
		    "ass124","lnk124","pos124","una124")

op55_agg3<-op55_agg2
op55_agg3$pos_tot[which(op55_agg3$pos_tot==0)]<-NA
op55_agg3$lnk_tot<-op55_agg3$ass_tot/op55_agg3$pos_tot
names(op55_agg3)[10:109]<-c(paste("ass",100:124,sep=""),paste("una",100:124,sep=""),paste("pos",100:124,sep=""),paste("lnk",100:124,sep=""))
op55_agg3<-op55_agg3[,c("row","fyq","date","fyear_qtr",
		  "snu1","snu2","snu1uid","psnuuid",
		  "ass_tot","una_tot","pos_tot","lnk_tot",
		  varing_var)]

snu55_long<-reshape(data=op55_agg3,idvar=c("psnuuid","fyq"),varying=varing_var,sep="",
					timevar="sex_age",times=1:25,new.row.names=NULL,direction="long")


row.names(snu55_long)<-NULL
names(snu55_long)[1]<-"index"
snu55_long<-cbind(row=1:nrow(snu55_long),snu55_long)
snu55_long$lnk_tot<-snu55_long$lnk<-NULL
snu55_long$ass_tot<-snu55_long$una_tot<-snu55_long$pos_tot<-NULL

#*** recode ***#
snu55_long2<-Recode(old.vars=sex_age,new.vars="sexage", old=100:124, new=agesex2,data=snu55_long)
snu55_long2$lnk<-snu55_long2$ass/snu55_long2$pos
snu55_long2<-snu55_long2[,c("row","index","fyq","snu1","snu2","sexage","ass","una","pos","lnk","sex_age","date","fyear_qtr","snu1uid","psnuuid")]

psnu55_long<-merge(snu2b[,c("objectid_1","level5name","snu1uid","uid","uid_diff","psnuuid","statename","name_1")],
			    snu55_long2[,c("row","index","fyq","snu1","snu2","sexage","ass","una","pos","lnk","sex_age","date","fyear_qtr","psnuuid")],
			    by="psnuuid",all.x=T,duplicateGeoms=T,sort=F)
psnu55_long@data$lon_cent<-centroid(psnu55_long)[,1];psnu55_long@data$lat_cent<-centroid(psnu55_long)[,2];
psnu55_long<-psnu55_long[,c("row","index","fyq","fyear_qtr","snu1","snu2","sexage","ass","una","pos","lnk","sex_age","date",
						  "psnuuid","objectid_1","level5name","snu1uid","uid","uid_diff","statename","name_1","lon_cent","lat_cent")]

psnu55_long2<-psnu55_long
splist55<-vector("list", length(psnu55_long2))
for (i in 1:length(splist55))
{
	splist55[[i]] <- psnu55_long2[psnu55_long2$row==i,]
	if(i%%1000==0) {print(i)}
	row.names(splist55[[i]])<-as.character(i)
}

psnu55_long3 <- do.call("rbind", splist55)
writeOGR(obj=psnu55_long3, dsn="./shapefiles/results/fy2019_snu55_long.shp",layer="snu55_long",driver="ESRI Shapefile",overwrite=T) # this is in geographical projection
psnu55_long_pt<-SpatialPointsDataFrame(psnu55_long3@data[,c("lon_cent","lat_cent")],psnu55_long3@data,proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
writeOGR(obj=psnu55_long_pt, dsn="./shapefiles/results/fy2019_snu55_long_pt.shp",layer="snu55_long_pt",driver="ESRI Shapefile",overwrite=T) # this is in geographical projection

#****************************************************************#
#****************************************************************#
### Facility: create shapefile ####
#****************************************************************#
#****************************************************************#
names(sites_fy19)[20:119]<-c(paste("ass",100:124,sep=""),paste("una",100:124,sep=""),paste("pos",100:124,sep=""),paste("lnk",100:124,sep=""))
sites_fy19<-sites_fy19[,c("row","sitetype2","uniqueid","fyq","date","fyear_qtr",
		  "sitename","snu1","snu2","psnu","snu1uid","psnuuid","snu2uid","orgunituid",
		  "lon","lat","ass_tot","una_tot","pos_tot","lnk_tot",
		  varing_var)]

sites_fy19_long<-reshape(data=sites_fy19,idvar=c("uniqueid","fyq"),varying=varing_var,sep="",
					timevar="sex_age",times=1:25,new.row.names=NULL,direction="long"
)

row.names(sites_fy19_long)<-NULL
names(sites_fy19_long)[1]<-"index"
sites_fy19_long<-cbind(row=1:nrow(sites_fy19_long),sites_fy19_long)
sites_fy19_long$lnk_tot<-sites_fy19_long$lnk<-NULL
sites_fy19_long$ass_tot<-sites_fy19_long$una_tot<-sites_fy19_long$pos_tot<-NULL
sites_fy19_long$lnk<-sites_fy19_long$ass/sites_fy19_long$pos

#*** recode sexo to numeric
sites_fy19_long2<-Recode(old.vars=sex_age,new.vars="sexage", old=100:124, new=agesex2,data=sites_fy19_long)
sites_fy19_long2<-sites_fy19_long2[,c("row","index","sitetype2","uniqueid","fyq","fyear_qtr","sexage","ass","una","pos","lnk","sex_age",
							   "snu1","snu2","psnu","sitename",
							   "date",
							   "snu1uid","psnuuid","snu2uid","orgunituid",
							   "lon","lat")]



sites_long_fy19_pt<-SpatialPointsDataFrame(sites_fy19_long2[,c("lon","lat")],sites_fy19_long2,proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
writeOGR(obj=sites_long_fy19_pt, dsn="./shapefiles/results/fy2019_sites55_long_2019-08-19.shp",layer="sites55_long",driver="ESRI Shapefile",overwrite=T) # this is in geographical projection




#####################
