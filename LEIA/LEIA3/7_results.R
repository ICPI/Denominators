#****************************************************************#
#****************************************************************#
### 7. results ######
#****************************************************************#
#****************************************************************#
#*** Assigned ***#
pa55_list<-lapply(1:2,function(i,x) do.call("rbind", x[[i]]),x=pa55)
pa55<-lapply(1:2,function(i,x) with(x[[i]],aggregate(x[[i]][,txnew_vars],by=list(uid=site_uid),FUN=sum,na.rm=T)),x=pa55_list)
pa55<-lapply(1:2,function(i,x) cbind(fyq=i,x[[i]]),x=pa55)
pa55_tot<-lapply(1:2, function(i,x) sum(x[[i]][,txnew_vars]),x=pa55)
pa55_fyq<-do.call("rbind",pa55)

#*** carryover
pc55_tot<-lapply(1:2, function(i,x) sum(x[[i]][,hts_pos_vars]),x=pc55)
pc55_fyq<-do.call("rbind",pc55)
row.names(pc55_fyq)<-NULL

#*** unassigned
pu55_tot<-lapply(1:2, function(i,x) sum(x[[i]][,hts_pos_vars]),x=pu55)
pu55_fyq<-do.call("rbind",pu55)
row.names(pu55_fyq)<-NULL

#*** unallocated
nu_tot<-lapply(1:2, function(i,x) sum(x[[i]][,txnew_vars]),x=nu55)
nu55_fyq<-do.call("rbind",nu55)
row.names(nu55_fyq)<-NULL

names(pa55_list)<-names(pa55)<-names(pc55_tot)<-names(pa55_tot)<-names(pu55_tot)<-names(nu_tot)<-c("fy19q1","fy19q2")


#****************************************************************#
#****************************************************************#
### output matrices ######
#****************************************************************#
#****************************************************************#

#*** Assigned ***#
x1<-pa55_fyq	#*** Assigned
names(x1)<-c("fyq","uid",
		   "ass1.0001","ass1.0104","ass1.0509","ass1.1014","ass1.1519","ass1.2024","ass1.2529","ass1.3034","ass1.3539","ass1.4044","ass1.4549","ass1.5000",
		   "ass2.0001","ass2.0104","ass2.0509","ass2.1014","ass2.1519","ass2.2024","ass2.2529","ass2.3034","ass2.3539","ass2.4044","ass2.4549","ass2.5000",
		   "ass3.9999")
x1$ass_tot<-apply(x1[,c("ass1.0001","ass1.0104","ass1.0509","ass1.1014","ass1.1519","ass1.2024","ass1.2529","ass1.3034","ass1.3539","ass1.4044","ass1.4549","ass1.5000",
				    "ass2.0001","ass2.0104","ass2.0509","ass2.1014","ass2.1519","ass2.2024","ass2.2529","ass2.3034","ass2.3539","ass2.4044","ass2.4549","ass2.5000",
				    "ass3.9999")],1,sum,na.rm=T)
#*********************************#
#*** carryover ***#
x2<-pc55_fyq	#*** carryover
names(x2)<-c("una1.0001","una1.0104","una1.0509","una1.1014","una1.1519","una1.2024","una1.2529","una1.3034","una1.3539","una1.4044","una1.4549","una1.5000",
		   "una2.0001","una2.0104","una2.0509","una2.1014","una2.1519","una2.2024","una2.2529","una2.3034","una2.3539","una2.4044","una2.4549","una2.5000",
		   "una3.9999",
		   "uid","fyq")
x2<-x2[,c("uid","fyq",
		"una1.0001","una1.0104","una1.0509","una1.1014","una1.1519","una1.2024","una1.2529","una1.3034","una1.3539","una1.4044","una1.4549","una1.5000",
		"una2.0001","una2.0104","una2.0509","una2.1014","una2.1519","una2.2024","una2.2529","una2.3034","una2.3539","una2.4044","una2.4549","una2.5000",
		"una3.9999")]
x2$una_tot<-apply(x2[,c("una1.0001","una1.0104","una1.0509","una1.1014","una1.1519","una1.2024","una1.2529","una1.3034","una1.3539","una1.4044","una1.4549","una1.5000",
				    "una2.0001","una2.0104","una2.0509","una2.1014","una2.1519","una2.2024","una2.2529","una2.3034","una2.3539","una2.4044","una2.4549","una2.5000",
				    "una3.9999")],1,sum,na.rm=T)
x2$pos_tot<-0
#*********************************#
x3<-pu55_fyq	#*** unassigned
names(x3)<-c("una1.0001","una1.0104","una1.0509","una1.1014","una1.1519","una1.2024","una1.2529","una1.3034","una1.3539","una1.4044","una1.4549","una1.5000",
		   "una2.0001","una2.0104","una2.0509","una2.1014","una2.1519","una2.2024","una2.2529","una2.3034","una2.3539","una2.4044","una2.4549","una2.5000",
		   "una3.9999",
		   "uid","fyq","una_tot","pos_tot")
x3<-x3[,c("uid","fyq",
		"una1.0001","una1.0104","una1.0509","una1.1014","una1.1519","una1.2024","una1.2529","una1.3034","una1.3539","una1.4044","una1.4549","una1.5000",
		"una2.0001","una2.0104","una2.0509","una2.1014","una2.1519","una2.2024","una2.2529","una2.3034","una2.3539","una2.4044","una2.4549","una2.5000",
		"una3.9999",
		"una_tot","pos_tot")]
x4<-x2;
x4[,-c(1,2)]<-x4[,-c(1,2)]+x3[,-c(1,2)];
#*********************************#
#*** unallocated ***#
x5<-nu55_fyq	#*** unallocated
names(x5)<-c("nal1.0001","nal1.0104","nal1.0509","nal1.1014","nal1.1519","nal1.2024","nal1.2529","nal1.3034","nal1.3539","nal1.4044","nal1.4549","nal1.5",
		   "nal2.0001","nal2.0104","nal2.0509","nal2.1014","nal2.1519","nal2.2024","nal2.2529","nal2.3034","nal2.3539","nal2.4044","nal2.4549","nal2.5",
		   "nal3.9999",
		   "uid","fyq","nal_tot","new_tot")
x5<-x5[,c("uid","fyq",
		"nal1.0001","nal1.0104","nal1.0509","nal1.1014","nal1.1519","nal1.2024","nal1.2529","nal1.3034","nal1.3539","nal1.4044","nal1.4549","nal1.5",
		"nal2.0001","nal2.0104","nal2.0509","nal2.1014","nal2.1519","nal2.2024","nal2.2529","nal2.3034","nal2.3539","nal2.4044","nal2.4549","nal2.5",
		"nal3.9999",
		"nal_tot","new_tot")]

output55<-cbind(x1,x4[,-c(1,2)]); output55<-output55[,output.names];


#****************************************************************#
rm(x1,x2,x3,x4,x5,n)




