snu1<-snu1b
snu2_cent <- centroid(snu2)
snu2_cent<-as.data.frame(snu2_cent)
names(snu2_cent)<-c("lon_cent","lat_cent")
snu2@data<-cbind(snu2@data,snu2_cent)
#****************************************************************#
#****************************************************************#
### snu2 aggregates ####
#****************************************************************#
#****************************************************************#
output55$tot1.0001<-output55$ass1.0001+output55$una1.0001;output55$tot1.0104<-output55$ass1.0104+output55$una1.0104;
output55$tot1.0509<-output55$ass1.0509+output55$una1.0509;output55$tot1.1014<-output55$ass1.1014+output55$una1.1014;
output55$tot1.1519<-output55$ass1.1519+output55$una1.1519;output55$tot1.2024<-output55$ass1.2024+output55$una1.2024;
output55$tot1.2529<-output55$ass1.2529+output55$una1.2529;output55$tot1.3034<-output55$ass1.3034+output55$una1.3034;
output55$tot1.3539<-output55$ass1.3539+output55$una1.3539;output55$tot1.4044<-output55$ass1.4044+output55$una1.4044;
output55$tot1.4549<-output55$ass1.4549+output55$una1.4549;output55$tot1.5000<-output55$ass1.5000+output55$una1.5000;
output55$tot2.0001<-output55$ass2.0001+output55$una2.0001;output55$tot2.0104<-output55$ass2.0104+output55$una2.0104;
output55$tot2.0509<-output55$ass2.0509+output55$una2.0509;output55$tot2.1014<-output55$ass2.1014+output55$una2.1014;
output55$tot2.1519<-output55$ass2.1519+output55$una2.1519;output55$tot2.2024<-output55$ass2.2024+output55$una2.2024;
output55$tot2.2529<-output55$ass2.2529+output55$una2.2529;output55$tot2.3034<-output55$ass2.3034+output55$una2.3034;
output55$tot2.3539<-output55$ass2.3539+output55$una2.3539;output55$tot2.4044<-output55$ass2.4044+output55$una2.4044;
output55$tot2.4549<-output55$ass2.4549+output55$una2.4549;output55$tot2.5000<-output55$ass2.5000+output55$una2.5000;
output55$tot3.9999<-output55$ass3.9999+output55$una3.9999;

ind<-(1:ncol(output55))*names(output55)%in%c("tot1.0001","tot1.0104","tot1.0509","tot1.1014","tot1.1519","tot1.2024","tot1.2529","tot1.3034","tot1.3539","tot1.4044","tot1.4549","tot1.5000",
  "tot2.0001","tot2.0104","tot2.0509","tot2.1014","tot2.1519","tot2.2024","tot2.2529","tot2.3034","tot2.3539","tot2.4044","tot2.4549","tot2.5000",
  "tot3.9999")
ind<-ind[ind>0]
for(i in ind)
{
	output55[[i]][which(output55[[i]]==0)]<-NA
}

output55$lnk1.0001<-output55$ass1.0001/output55$tot1.0001;output55$lnk1.0104<-output55$ass1.0104/output55$tot1.0104;
output55$lnk1.0509<-output55$ass1.0509/output55$tot1.0509;output55$lnk1.1014<-output55$ass1.1014/output55$tot1.1014;
output55$lnk1.1519<-output55$ass1.1519/output55$tot1.1519;output55$lnk1.2024<-output55$ass1.2024/output55$tot1.2024;
output55$lnk1.2529<-output55$ass1.2529/output55$tot1.2529;output55$lnk1.3034<-output55$ass1.3034/output55$tot1.3034;
output55$lnk1.3539<-output55$ass1.3539/output55$tot1.3539;output55$lnk1.4044<-output55$ass1.4044/output55$tot1.4044;
output55$lnk1.4549<-output55$ass1.4549/output55$tot1.4549;output55$lnk1.5000<-output55$ass1.5000/output55$tot1.5000;
output55$lnk2.0001<-output55$ass2.0001/output55$tot2.0001;output55$lnk2.0104<-output55$ass2.0104/output55$tot2.0104;
output55$lnk2.0509<-output55$ass2.0509/output55$tot2.0509;output55$lnk2.1014<-output55$ass2.1014/output55$tot2.1014;
output55$lnk2.1519<-output55$ass2.1519/output55$tot2.1519;output55$lnk2.2024<-output55$ass2.2024/output55$tot2.2024;
output55$lnk2.2529<-output55$ass2.2529/output55$tot2.2529;output55$lnk2.3034<-output55$ass2.3034/output55$tot2.3034;
output55$lnk2.3539<-output55$ass2.3539/output55$tot2.3539;output55$lnk2.4044<-output55$ass2.4044/output55$tot2.4044;
output55$lnk2.4549<-output55$ass2.4549/output55$tot2.4549;output55$lnk2.5000<-output55$ass2.5000/output55$tot2.5000;
output55$lnk3.9999<-output55$ass3.9999/output55$tot3.9999;


names(output55)[2]<-"uniqueid"
# op55<-merge(output55,fac_gps[,c("uniqueid","sitetype2","sitename","snu1","snu2","psnu","snu1uid","psnuuid","snu2uid","orgunituid")],by="uniqueid",all.x=T,sort=F)
op55<-merge(output55,fac_gps[,c("uniqueid","sitetype2","sitename","snu1","snu2","psnu","snu1uid","psnuuid","snu2uid","orgunituid")],by="uniqueid",all.x=T,sort=F)

op55<-op55[order(op55$fyq,op55$uniqueid),]
row.names(op55)<-NULL

op55_agg<-with(op55,aggregate(op55[,result_vars],by=list(fyq=fyq,snu1=snu1,snu2=snu2,snu1uid=snu1uid,psnuuid=psnuuid,snu2uid=snu2uid),sum,na.rm=T))

row.names(op55_agg)<-NULL;

ind<-which(op55_agg$pos_tot==0)
op55_agg$pos_tot[ind]<-NA


ind<-(1:ncol(op55_agg))*names(op55_agg)%in%c("tot1.0001","tot1.0104","tot1.0509","tot1.1014","tot1.1519","tot1.2024","tot1.2529","tot1.3034","tot1.3539","tot1.4044","tot1.4549","tot1.5000",
									    "tot2.0001","tot2.0104","tot2.0509","tot2.1014","tot2.1519","tot2.2024","tot2.2529","tot2.3034","tot2.3539","tot2.4044","tot2.4549","tot2.5000",
									    "tot3.9999")
ind<-ind[ind>0]
for(i in ind)
{
	op55_agg[[i]][which(op55_agg[[i]]==0)]<-NA
}

op55_agg$lnk1.0001<-op55_agg$ass1.0001/op55_agg$tot1.0001;op55_agg$lnk1.0104<-op55_agg$ass1.0104/op55_agg$tot1.0104;
op55_agg$lnk1.0509<-op55_agg$ass1.0509/op55_agg$tot1.0509;op55_agg$lnk1.1014<-op55_agg$ass1.1014/op55_agg$tot1.1014;
op55_agg$lnk1.1519<-op55_agg$ass1.1519/op55_agg$tot1.1519;op55_agg$lnk1.2024<-op55_agg$ass1.2024/op55_agg$tot1.2024;
op55_agg$lnk1.2529<-op55_agg$ass1.2529/op55_agg$tot1.2529;op55_agg$lnk1.3034<-op55_agg$ass1.3034/op55_agg$tot1.3034;
op55_agg$lnk1.3539<-op55_agg$ass1.3539/op55_agg$tot1.3539;op55_agg$lnk1.4044<-op55_agg$ass1.4044/op55_agg$tot1.4044;
op55_agg$lnk1.4549<-op55_agg$ass1.4549/op55_agg$tot1.4549;op55_agg$lnk1.5000<-op55_agg$ass1.5000/op55_agg$tot1.5000;
op55_agg$lnk2.0001<-op55_agg$ass2.0001/op55_agg$tot2.0001;op55_agg$lnk2.0104<-op55_agg$ass2.0104/op55_agg$tot2.0104;
op55_agg$lnk2.0509<-op55_agg$ass2.0509/op55_agg$tot2.0509;op55_agg$lnk2.1014<-op55_agg$ass2.1014/op55_agg$tot2.1014;
op55_agg$lnk2.1519<-op55_agg$ass2.1519/op55_agg$tot2.1519;op55_agg$lnk2.2024<-op55_agg$ass2.2024/op55_agg$tot2.2024;
op55_agg$lnk2.2529<-op55_agg$ass2.2529/op55_agg$tot2.2529;op55_agg$lnk2.3034<-op55_agg$ass2.3034/op55_agg$tot2.3034;
op55_agg$lnk2.3539<-op55_agg$ass2.3539/op55_agg$tot2.3539;op55_agg$lnk2.4044<-op55_agg$ass2.4044/op55_agg$tot2.4044;
op55_agg$lnk2.4549<-op55_agg$ass2.4549/op55_agg$tot2.4549;op55_agg$lnk2.5000<-op55_agg$ass2.5000/op55_agg$tot2.5000;
op55_agg$lnk3.9999<-op55_agg$ass3.9999/op55_agg$tot3.9999;

op55_agg<-op55_agg[order(op55_agg$fyq,op55_agg$snu1,op55_agg$snu2),]	# snu2 results for the two quarters
row.names(op55_agg)<-NULL

#****************************************************************#
#****************************************************************#
### create SNU2 results shapefile ####
#****************************************************************#
#****************************************************************#
#*** merge snu2s with no data to op55_agg***#
psnu<-psnu@data
op55_snu2<-op55_agg[!duplicated(op55_agg$psnuuid),]
op55_snu2$linked<-1
snu2_na<-merge(psnu[,c("psnuuid","level5name")],op55_snu2[,-c(1:4,6)],by="psnuuid",all.x=T,sort=F)
snu2_na<-snu2_na[-which(snu2_na$linked==1),-2]
snu2_na$linked<-NULL
snu2_na<-merge(snu2_na,op55_snu2[,c("snu1","snu2","snu1uid","psnuuid","snu2uid")],by="psnuuid",all.x=T,sort=F)
snu2_na<-rbind(cbind(fyq=1,snu2_na),cbind(fyq=2,snu2_na))
snu2_na<-snu2_na[,names(op55_agg)]

fyq<-read.csv("./data/fyq_code.csv",stringsAsFactors=F)
fyq$date<-as.Date(fyq$date,"%m/%d/%Y");
fyq<-fyq[fyq$fyq>8,c("fyq","date","fyear_qtr")]
fyq$fyq<-1:2
row.names(fyq)<-NULL

op55_agg2<-rbind(op55_agg,snu2_na)

op55_agg2<-op55_agg2[order(op55_agg2$fyq,op55_agg2$snu1,op55_agg2$snu2),]
row.names(op55_agg2)<-NULL

op55_agg2<-cbind(row=1:nrow(op55_agg2),op55_agg2)

op55_agg2<-merge(op55_agg2,fyq,by="fyq",all.x=T,sort=F)
op55_agg2<-op55_agg2[,c(2,1,3:ncol(op55_agg2))]
op55_agg2$snu2uid<-NULL

#****************************************************************#
snu2b<-subset(snu2,subset=(snu1!="Lake Chad"))
vars.index<-names(op55_agg2)[!(names(op55_agg2)%in%c("snu1","snu2","snu1uid"))]
psnu55<-merge(snu2b,op55_agg2[,vars.index],by="psnuuid",all.x=T,duplicateGeoms=T,sort=F)
psnu55<-psnu55[,psnu_vars]


#****************************************************************#
#****************************************************************#
### rename testing fields ####
#****************************************************************#
#****************************************************************#

psnu55b<-psnu55
names(psnu55b)[psnu_vars%in%pos_lnk]<-paste("linkage_",agesex,sep="")
names(psnu55b)[psnu_vars%in%pos_ass]<-paste("assigned_",agesex,sep="")
names(psnu55b)[psnu_vars%in%pos_una]<-paste("unassigned_",agesex,sep="")
names(psnu55b)[psnu_vars%in%pos_tot]<-paste("totpos_",agesex,sep="")

### sort shapefile #####
splist55<-vector("list", length(psnu55b))
for (i in 1:length(splist55))
{
	splist55[[i]] <- psnu55b[psnu55b$row==i,]
	row.names(splist55[[i]])<-as.character(i)
}

psnu55c <- do.call("rbind", splist55)


writeOGR(obj=psnu55c, dsn="./shapefiles/results/fy2019_sun2_abm55_2019-08-28.shp",layer="sun2_abm55",driver="ESRI Shapefile",overwrite=T) # this is in geographical projection

psnu55_cent<-data.frame(centroid(psnu55c));names(psnu55_cent)<-c("lon_cent","lat_cent");

psnu55_pt<-SpatialPointsDataFrame(psnu55_cent,psnu55c@data,proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))


writeOGR(obj=psnu55_pt, dsn="./shapefiles/results/fy2019_sun2_abm55_pt_2019-08-28.shp",layer="sun2_abm55_pt",driver="ESRI Shapefile",overwrite=T) # this is in geographical projection


