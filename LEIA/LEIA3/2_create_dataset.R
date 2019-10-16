#****************************************************************#
#****************************************************************#
### 2. Create dataset ######
#****************************************************************#
#****************************************************************#
type<-c(rep("character",43),rep("numeric",7))
ou<-read.delim("MER_Structured_Dataset_Site_IM_FY17-19_20190621_v2_1_Nigeria.txt",header=T,colClasses=type,stringsAsFactors=F)
names(ou)[1]<-"orgUnitUID"; names(ou)<-tolower(names(ou))
index<-which(ou$indicator %in% c("HTS_TST_POS","TX_NEW","TX_CURR"))

nigeria<-ou[index,];
nigeria<-nigeria[nigeria$sitetype!="Military",];

#****************************************************************#
#****************************************************************#
### Reshape and aggregate data ######
#****************************************************************#
#****************************************************************#
nigeria2<-nigeria[nigeria$fiscal_year==2019,]

ageasentered<-read.csv("ageasentered.csv",stringsAsFactors=F)
nigeria2<-merge(nigeria2,ageasentered,by="ageasentered",all.x=T,sort=F)
nigeria2$ageasentered2<-nigeria2$ageasentered


nigeria2$code1[which(is.na(nigeria2$code1))]<-9999;
nigeria2$code2[which(is.na(nigeria2$code2))]<-999;
nigeria2$agegrp[which(is.na(nigeria2$agegrp))]<-"Unknown Age";
nigeria2$ageasentered2[which(nigeria2$ageasentered2=="")]<-"Unknown Age";
nigeria2$gender<-ifelse(nigeria2$sex=="Male",1,ifelse(nigeria2$sex=="Female",2,3))

nigeria2$sex_age1<-nigeria2$gender+nigeria2$code1/10000
nigeria2$sex_age2<-nigeria2$gender+nigeria2$code2/1000

nigeria2$sex2<-nigeria2$sex
nigeria2$sex2[which(nigeria2$sex2=="")]<-"Unknown Sex";
nigeria2$sex_age3<-paste(nigeria2$sex2,"ages",nigeria2$ageasentered2)

#****************************************************************#
#****************************************************************#
### Aggregate MER site data ####
#****************************************************************#
#****************************************************************#
facility<-with(nigeria2[nigeria2$sitetype=="Facility",],aggregate(nigeria2[nigeria2$sitetype=="Facility",c("qtr1","qtr2")],
														  by=list(orgunituid=orgunituid,fyear=fiscal_year,indicator=indicator,sex_age1=sex_age1,sitetype=sitetype),
														  FUN=sum,na.rm=T))
community<-with(nigeria2[nigeria2$sitetype=="Community",],aggregate(nigeria2[nigeria2$sitetype=="Community",c("qtr1","qtr2")],
														    by=list(orgunituid=orgunituid,fyear=fiscal_year,indicator=indicator,sex_age1=sex_age1,sitetype=sitetype),
														    FUN=sum,na.rm=T))

#****************************************************************#
#****************************************************************#
### Reshape MER facility sites data ####
#****************************************************************#
#****************************************************************#
fac_long<-reshape(facility,idvar=c("orgunituid","fyear","indicator","sex_age1"),varying=c("qtr1","qtr2"),
				  v.names="total",timevar="qtr",time=1:2,
				  direction="long")
row.names(fac_long)<-NULL
fac_long$pos<-fac_long$total*(fac_long$indicator=="HTS_TST_POS")
fac_long$new<-fac_long$total*(fac_long$indicator=="TX_NEW")
fac_long$cur<-fac_long$total*(fac_long$indicator=="TX_CURR")

#** remove sites without data
index<-which(fac_long$total==0)
fac_long<-fac_long[-index,]

fac_agg<-with(fac_long,aggregate(fac_long[,c("total","pos","new","cur")],
								  by=list(orgunituid=orgunituid,fyear=fyear,qtr=qtr,sex_age1=sex_age1,sitetype=sitetype),
								  FUN=sum,na.rm=T))
fac_agg$total<-NULL

fac_wide<-reshape(fac_agg,idvar=c("orgunituid","fyear","qtr","sitetype"),v.names=c("pos","new","cur"),timevar="sex_age1",direction="wide")
fac_wide$fyear_qtr<-fac_wide$fyear+fac_wide$qtr/10
fac_wide<-Recode(old.vars=fyear_qtr,new.vars="fyq", old=c(2019.1,2019.2), new=1:2,data=fac_wide);

if(length(setdiff(abm_vars2,names(fac_wide)))>0)
{
	fac_NA<-as.data.frame(matrix(nrow=nrow(fac_wide),ncol=length(setdiff(abm_vars2,names(fac_wide))),dimnames=list(NULL,setdiff(abm_vars2,names(fac_wide)))))
	fac_wide<-cbind(fac_wide,fac_NA)
}

fac_wide<-fac_wide[,abm_vars2]
names(fac_wide)<-abm_vars
fac_wide<-fac_wide[order(fac_wide$fyq,fac_wide$orgunituid),]
row.names(fac_wide)<-NULL

#****************************************************************#
#****************************************************************#
### Reshape MER Community sites data ####
#****************************************************************#
#****************************************************************#
com_long<-reshape(community,idvar=c("orgunituid","fyear","indicator","sex_age1"),varying=c("qtr1","qtr2"),
				  v.names="total",timevar="qtr",time=1:2,
				  direction="long")
row.names(com_long)<-NULL
com_long$pos<-com_long$total*(com_long$indicator=="HTS_TST_POS")
com_long$new<-com_long$total*(com_long$indicator=="TX_NEW")
com_long$cur<-com_long$total*(com_long$indicator=="TX_cur")

#** remove sites without data
index<-which(com_long$total==0)
com_long<-com_long[-index,]
com_long$total<-NULL

com_agg<-with(com_long,aggregate(com_long[,c("pos","new","cur")],
								  by=list(orgunituid=orgunituid,fyear=fyear,qtr=qtr,sex_age1=sex_age1,sitetype=sitetype),
								  FUN=sum,na.rm=T))

com_wide<-reshape(com_agg,idvar=c("orgunituid","fyear","qtr","sitetype"),v.names=c("pos","new","cur"),timevar="sex_age1",direction="wide")
com_wide$fyear_qtr<-com_wide$fyear+com_wide$qtr/10
com_wide<-Recode(old.vars=fyear_qtr,new.vars="fyq", old=c(2019.1,2019.2), new=1:2,data=com_wide);

if(length(setdiff(abm_vars2,names(com_wide)))>0)
{
	com_NA<-as.data.frame(matrix(nrow=nrow(com_wide),ncol=length(setdiff(abm_vars2,names(com_wide))),dimnames=list(NULL,setdiff(abm_vars2,names(com_wide)))))
	com_wide<-cbind(com_wide,com_NA)
}

com_wide<-com_wide[,abm_vars2]
names(com_wide)<-abm_vars

com_wide<-com_wide[order(com_wide$fyq,com_wide$orgunituid),]
row.names(com_wide)<-NULL

#****************************************************************#
#****************************************************************#
### Combine MER facility and community sites ####
#****************************************************************#
#****************************************************************#
index<-(1:ncol(fac_wide))*(names(fac_wide)%in%indicator_vars)
index<-index[index>0]

fac_wide2<-fac_wide;fac_wide2$sitetype2<-1;
com_wide2<-com_wide;com_wide2$sitetype2<-2;
facilities<-rbind(fac_wide2,com_wide2)

for(i in index)
{
	ind<-which(is.na(facilities[[i]]))
	facilities[[i]][ind]<-0

}

#********************************************************#

facilities$pos_tot<-apply(facilities[,hts_pos_vars],1,sum)
facilities$new_tot<-apply(facilities[,txnew_vars],1,sum)
facilities$cur_tot<-apply(facilities[,txcurr_vars],1,sum)

facilities<-facilities[,c("sitetype2","orgunituid","fyq","pos_tot","new_tot","cur_tot",hts_pos_vars,txnew_vars,txcurr_vars,"fyear_qtr","fyear","qtr","sitetype")]

index<-which((facilities$pos_tot==0)&(facilities$new_tot==0))

facilities<-facilities[-index,]
