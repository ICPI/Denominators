#****************************************************************#
#****************************************************************#
### Import shapefiles ####
#****************************************************************#
#****************************************************************#
sites<-readOGR("./shapefiles/nigeria_facilities.shp",stringsAsFactors=F);
names(sites@data)<-tolower(names(sites@data));
sites@data<-sites@data[,c("field1","uid","level4name","level5name","name","uidlevel4","uidlevel5","longitude","latitude")]

psnu<-readOGR("./shapefiles/Nigeria_PROD_5_LGA_LocalGovernmentAreaLsib_2017_Mar.shp",stringsAsFactors=F);
names(psnu@data)<-tolower(names(psnu@data));
psnu@data$objectid<-psnu@data$organisati<-psnu@data$code<-psnu@data$level<-psnu@data$level1name<-psnu@data$level2name<-psnu@data$level3name<-NULL
psnu@data$level4name<-psnu@data$level5name<-psnu@data$name<-NULL

#*** correct uid format error ***#
psnu@data[255,"uid"]<-"WFsEvMCIS56"	# "WFsEvMCIS56\r\nWFsEvMCIS56\r\n"
psnu@data[632,"uid"]<-"tNxqnAq8Xc0"	# "tNxqnAq8Xc0\r\ntNxqnAq8Xc0\r\ntNxqnAq8Xc0\r\n"

#****************************************************************#
### Correct incorrect snu2ids ####
#****************************************************************#
# "Nigeria_PROD_5_LGA_LocalGovernmentAreaLsib_2017_Mar.shp" file #
# has six incorrect uid codes (uidlevel5) that do not match	the	#
# psnuuid code in the MER data.							#
# File "psnu_crosswalk_2019-08-28.csv" is used to link the snu2s.#
# uid_diff=1 for incorrect uid codes in the PEPFAR shapefile.	#
#****************************************************************#

psnu2<-read.csv("psnu_crosswalk_2019-08-28.csv",stringsAsFactors=F)
names(psnu2)<-tolower(names(psnu2));
psnu2$uid_diff[which(is.na(psnu2$uid_diff))]<-0

psnu@data<-merge(psnu@data,psnu2[,c("snu1uid","snu2","level4name","level5name","uid","uid_diff","psnuuid")],by="uid",all.x=T,sort=F)
psnu@data<-psnu@data[,c("objectid_1","level4name","snu2","level5name","snu1uid","uid","uid_diff","psnuuid","statename","name_1")]

lake<-readOGR("./shapefiles/lake_chad_africa_waterbody.shp",stringsAsFactors=F);
names(lake@data)<-tolower(names(lake@data));
#****************************************************************#

#****************************************************************#
#****************************************************************#
### Create SNU1: Merge psnu to snu1 #####
#****************************************************************#
#****************************************************************#
psnu.id <- psnu@data$statename
snu1<-unionSpatialPolygons(psnu, psnu.id)
snu.df<-data.frame(snu1=sort(unique(psnu@data$statename)))
row.names(snu.df)<-snu.df$snu1
snu1 <- SpatialPolygonsDataFrame(snu1, snu.df)

#****************************************************************#
#****************************************************************#
### Crop Lake Chad from snu1 Borno #####
#****************************************************************#
# The PEPFAR shapefile includes Lake Chad as a land mass		#
#****************************************************************#
#****************************************************************#
lake$af_wtr_id<-lake$sqkm<-lake$type_of_wa<-lake$shape_area<-lake$shape_len<-NULL
names(lake@data)<-"snu1"
lake@data$snu1<-"Lake Chad"

lake1<-intersect(snu1[8,],lake)
borno <- symdif(snu1[8,], lake1);		# raster package: creates SpatialPolygonsDataFrame
lake1@data$snu1.2<-NULL
names(lake1@data)<-"snu1"
lake1@data$snu1<-"Lake Chad"
row.names(lake1@data)<-"Lake Chad"; row.names(lake1)<-"Lake Chad";

splist<-vector("list", length(snu1)+1)
for (i in 1:length(snu1))
{

	splist[[i]] <- snu1[i,]
}
splist[[8]]<-borno[1,]
splist[[38]]<-lake1
snu1b <- do.call("rbind", splist)
#****************************************************************#
statenames<-psnu2[(!duplicated(psnu2$statename)),c("statename","snu1uid","level4name")]
statenames<-statenames[order(statenames$statename),]
statenames$statename[statenames$statename=="Nasarawa"]<-"Nassarawa"
row.names(statenames)<-NULL
snu1@data<-merge(snu1@data,statenames,by.x="snu1",by.y="statename",all.x=T,sort=F)
snu1b@data<-merge(snu1b@data,statenames,by.x="snu1",by.y="statename",all.x=T,sort=F)
snu1b@data$level4name[snu1b@data$snu1=="Lake Chad"]<-"Lake Chad"

#****************************************************************#
#****************************************************************#
### Crop Lake Chad from snu2 #####
#****************************************************************#
# The PEPFAR shapefile includes Lake Chad as a land mass which 	#
# results in snu2 centroids over Lake Chad					#
#****************************************************************#
#****************************************************************#
snu2<-psnu
snu2@data$objectid_1<-as.integer(snu2@data$objectid_1)
names(snu2@data)[2]<-"snu1"

# raster package: creates SpatialPolygonsDataFrame
Abadam<-intersect(snu2[162,],borno);Kukawa<-intersect(snu2[169,],borno);
Monguno<-intersect(snu2[176,],borno);Marte<-intersect(snu2[156,],borno);
Abadam@data$snu1.2<-NULL;names(Abadam@data)[2]<-"snu1";Kukawa@data$snu1.2<-NULL;names(Kukawa@data)[2]<-"snu1";
Monguno@data$snu1.2<-NULL;names(Monguno@data)[2]<-"snu1";Marte@data$snu1.2<-NULL;names(Marte@data)[2]<-"snu1";

psnu_ord<-order(snu2$snu1,snu2$snu2)
splist<-vector("list", length(snu2)+1)
for (i in 1:length(snu2))
{
	splist[[i]] <- snu2[psnu_ord[i],]
	row.names(splist[[i]])<-as.character(i-1)
	splist[[i]]@data$objectid_1<-i
}
row.names(Abadam)<-"141";row.names(Abadam@data)<-"141";row.names(Kukawa)<-"157";row.names(Kukawa@data)<-"157";
row.names(Monguno)<-"164";row.names(Monguno@data)<-"164";row.names(Marte)<-"162";row.names(Marte@data)<-"162";

splist[[142]]<-Abadam[1,];row.names(splist[[142]])<-"141";splist[[142]]@data$objectid_1<-142;
splist[[158]]<-Kukawa[1,];row.names(splist[[158]])<-"157";splist[[158]]@data$objectid_1<-158;
splist[[165]]<-Monguno[1,];row.names(splist[[165]])<-"164";splist[[165]]@data$objectid_1<-165;
splist[[163]]<-Marte[1,];row.names(splist[[163]])<-"162";splist[[163]]@data$objectid_1<-163;

lake2<-lake1
lake2@data$objectid_1<-777;
lake2@data$uid<-lake2@data$snu1uid<-lake2@data$psnuuid<-NA;
lake2@data$snu1<-lake2@data$snu2<-lake2@data$level5name<-lake2@data$statename<-lake2@data$name_1<-"Lake Chad";
lake2@data$uid_diff<-0
lake2@data<-lake2@data[,c("objectid_1","snu1","snu2","level5name","snu1uid","uid","uid_diff","psnuuid","statename","name_1")];
row.names(lake2@data)<-"774";row.names(lake2)<-"774";
splist[[length(snu2)+1]]<-lake2
snu2<-do.call("rbind", splist)



#****************************************************************#
#****************************************************************#
### Centroid: SNU1 ####
#****************************************************************#
#****************************************************************#
adm1<-subset(snu1b,subset=(snu1!="Lake Chad"))
adm1_cent <- centroid(adm1)
adm1_cent<-as.data.frame(adm1_cent)
names(adm1_cent)<-c("lon","lat")
adm1_cent<-cbind(adm1@data,adm1_cent)
snu1_cent<-SpatialPointsDataFrame(adm1_cent[,c("lon","lat")],adm1_cent,proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

#****************************************************************#
#****************************************************************#
### Centroid: SNU2 ####
#****************************************************************#
#****************************************************************#
adm2<-subset(snu2,subset=(snu2!="Lake Chad"))
adm2_cent <- centroid(adm2)
adm2_cent<-as.data.frame(adm2_cent)
names(adm2_cent)<-c("lon","lat")
adm2_cent<-cbind(adm2@data,adm2_cent)
snu2_cent<-SpatialPointsDataFrame(adm2_cent[,c("lon","lat")],adm2_cent,proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

#****************************************************************#
#****************************************************************#
### Write files ####
#****************************************************************#
#****************************************************************#
#*** snu1
writeOGR(obj=snu1, dsn="./shapefiles/snu1.shp",layer="snu1",driver="ESRI Shapefile",overwrite=T)
writeOGR(obj=snu1b, dsn="./shapefiles/snu1_lake_chad.shp",layer="snu1",driver="ESRI Shapefile",overwrite=T)
writeOGR(obj=snu1_cent, dsn="./shapefiles/snu1_cent.shp",layer="snu1_cent",driver="ESRI Shapefile",overwrite=T) # this is in geographical projection

#*** snu2
writeOGR(obj=snu2, dsn="./shapefiles/snu2_lake_chad.shp",layer="snu2",driver="ESRI Shapefile",overwrite=T) # this is in geographical projection
writeOGR(obj=snu2_cent, dsn="./shapefiles/snu2_cent.shp",layer="snu2_cent",driver="ESRI Shapefile",overwrite=T) # this is in geographical projection
