#****************************************************************#
#****************************************************************#
### 5. ABM  ######
#****************************************************************#
#****************************************************************#

radius<-55


### function ####
ouput<-function(w,site,neighs,agesex)
{
	op<-as.data.frame(matrix(0,nrow=length(neighs),ncol=length(agesex)+3,dimnames=list(NULL,c(agesex,"site_uid","neigh_uid","miles"))))
	op$site_uid<-site
	op$neigh_uid<-neighs
	op$miles<-as.numeric(w)
	return(op)
}
#****************************************************************#
#****************************************************************#
uniqueid<-fac_gps[,c("uniqueid","orgunituid")];
n<-nrow(uniqueid);
uid<-uniqueid$uniqueid;
uid.names<-c(paste("uid000",uid[1:9],sep=""),paste("uid00",uid[10:99],sep=""),paste("uid0",uid[100:999],sep=""),paste("uid",uid[1000:n],sep=""))
#****************************************************************#
#****************************************************************#
w0<-w.lonlat$w
com_index<-fac_gps$uniqueid[fac_gps$sitetype2==2]

# radius<-55
wdist<-ifelse(w0<=radius,w0,NA)	# get spatial neighbors matrix
diag(wdist)<-0
wdist[com_index,]<-NA

wdist<-as.data.frame(wdist); names(wdist)<-uid.names

neigh.index<-lapply(1:n,function(i,x,com_index) which(!is.na(x[[i]])),x=wdist,com_index=com_index)
names(neigh.index)<-uid.names
#****************************************************************#


### 6.1 add uniqueid to site data ######
#****************************************************************#

facilities2<-facilities

# facilities2$row<-1:nrow(facilities2)
row.names(facilities2)<-NULL

#*************************#
facilities2<-merge(facilities2,uniqueid,by="orgunituid",all.x=T,sort=F)
facilities2<-facilities2[order(facilities2$fyq,facilities2$uniqueid),
					c("uniqueid","fyq","pos_tot","new_tot",hts_pos_vars,txnew_vars,"fyear_qtr","fyear","qtr","sitetype2","orgunituid","sitetype","cur_tot",txcurr_vars)]
row.names(facilities2)<-NULL
#****************************************************************#
#****************************************************************#
### 6.2 testing data matrices ######
#****************************************************************#
#****************************************************************#
#*** quarterly pos and txnew matrices ***#
pos1<-split(facilities2[,c(hts_pos_vars,"uniqueid","fyq","pos_tot")],facilities2$fyq)
new1<-split(facilities2[,c(txnew_vars,"uniqueid","fyq","new_tot")],facilities2$fyq)
new.unallocated<-as.list(1:2)
names(pos1)<-names(new1)<-names(new.unallocated)<-c("fy19q1","fy19q2")
#****************************************************************#
#****************************************************************#
assigned<-lapply(1:length(neigh.index),function(i,w,x,lbl) ouput(w[i,x[[i]]],i,x[[i]],lbl),w=as.matrix(wdist),x=neigh.index,lbl=txnew_vars)		# assigned new initiates matrix
names(assigned)<-uid.names
assigned<-lapply(1:2,function(i,x) x,x=assigned)
names(assigned)<-c("fy19q1","fy19q2")
#****************************************************************#
carryover<-as.data.frame(matrix(0,nrow=2*nrow(uniqueid),ncol=(length(hts_pos_vars)+2),dimnames=list(NULL,c(hts_pos_vars,"uid","fyq"))))
carryover$uid<-rep(uniqueid$uniqueid,2)
carryover$fyq<-rep(1:2,each=nrow(uniqueid))
carryover<-split(carryover,carryover$fyq)
names(carryover)<-c("fy19q1","fy19q2")
for(j in 1:2){row.names(carryover[[j]])<-NULL}
unassigned<-carryover

#****************************************************************#
#****************************************************************#
### 6.3 simulation ######
#****************************************************************#
#****************************************************************#
ptm2a<- proc.time()

for(i in 1:length(pos1))
{	# loop through the 10 quarters

	#**** uid totals matrices ****#
	pos.m<-as.data.frame(matrix(0,nrow=length(uid),ncol=(length(hts_pos_vars)+3),dimnames=list(NULL,c(hts_pos_vars,"uniqueid","fyq","pos_tot"))))
	pos.m[pos1[[i]]$uniqueid,]<-pos1[[i]]; pos.m$uniqueid<-uid;	pos.m$pos_tot2<-pos.m$pos_tot;
	new.m<-as.data.frame(matrix(0,nrow=length(uid),ncol=(length(txnew_vars)+3),dimnames=list(NULL,c(txnew_vars,"uniqueid","fyq","new_tot"))))
	new.m[new1[[i]]$uniqueid,]<-new1[[i]]; new.m$uniqueid<-uid; new.m$new_tot2<-new.m$new_tot;
	#****************************************************************#

	#**** quarterly uid totals matrices ****#
	pos_tot<-pos1[[i]][,c("uniqueid","pos_tot")]
	pos_tot<-pos_tot[pos_tot$pos_tot>0,]

	new_tot<-new1[[i]][,c("uniqueid","new_tot")]
	new_tot<-new_tot[new_tot$new_tot>0,]
	#****************************************************************#
	pos_counter<-sum(pos_tot$pos_tot)
	new_counter<-sum(new_tot$new_tot)

	#****************************************************************#
	#****************************************************************#
	#**** 6.3.1 assign agent ######
	#****************************************************************#
	#****************************************************************#
	counter<-0
	while(counter<pos_counter)
	{
		counter<-counter+1
		if(counter%%10000==0) {print(counter/10000); print(paste("i = ",i,": ","new_counter = ",new_counter,sep=""))}
		#****************************************************************#
		#*** select site and age/sex classification ****#
		ind2<-0
		while(ind2==0)
		{	# select a facility and age/sex classification that has data
			if(length(pos_tot$uniqueid)>1)
			{
				ind1<-sample(pos_tot$uniqueid,1,prob=pos_tot$pos_tot/sum(pos_tot$pos_tot))
			} else if(length(pos_tot$uniqueid)==1)
			{
				ind1<-pos_tot$uniqueid
			}

			if(length(which(pos.m[ind1,hts_pos_vars]>0))>1)	# if true, facilty has positive results
			{
				pos_ind<-which(pos.m[ind1,hts_pos_vars]>0)
				pos_p<-as.numeric(pos.m[ind1,pos_ind]/sum(pos.m[ind1,pos_ind]))
				ind2<-sample(pos_ind,1,prob=pos_p)
			} else if(length(which(pos.m[ind1,hts_pos_vars]>0))==1)
			{
				ind2<-which(pos.m[ind1,hts_pos_vars]>0)
			}
			# print("loop")
		}
		#****************************************************************#
		if(i==1)
		{
			agent<-neigh(spw=wdist[ind1,neigh.index[[ind1]]],txnew=new.m,ind1=ind1,ind2=ind2,index=neigh.index[[ind1]],site_prob=1)
			#*** agent assigned or carried over to next quarter***#
			pos.m[agent$home_uid,ind2]<-pos.m[agent$home_uid,ind2]-1
			pos.m[agent$home_uid,"pos_tot"]<-pos.m[agent$home_uid,"pos_tot"]-1
			pos_tot$pos_tot[pos_tot$uniqueid==agent$home_uid]<-pos_tot$pos_tot[pos_tot$uniqueid==agent$home_uid]-1

			if(is.null(agent$neigh_uid))
			{	#agent not assigned
				# print(paste("i = ",i,": ","counter = ",counter,": agent carried over",sep=""))
				carryover[[i]][ind1,ind2]<-carryover[[i]][ind1,ind2]+1
				next
			}
			#*** agent was assigned ***#
			new_counter<-new_counter-1
			new.m[agent$neigh_uid,ind2]<-new.m[agent$neigh_uid,ind2]-1
			new.m[agent$neigh_uid,"new_tot"]<-new.m[agent$neigh_uid,"new_tot"]-1
			new_tot$new_tot[new_tot$uniqueid==agent$neigh_uid]<-new_tot$new_tot[new_tot$uniqueid==agent$neigh_uid]-1

			# home site positive assigned to neighbor
			assigned[[i]][[ind1]][assigned[[i]][[ind1]]$neigh_uid==agent$neigh_uid,ind2]<-assigned[[i]][[ind1]][assigned[[i]][[ind1]]$neigh_uid==agent$neigh_uid,ind2]+1

		} else
		{	# select either a person carried over from the previous quarter or someone from the current quarter
			if(carryover[[i-1]][ind1,ind2]>0)
			{	# selection probability: carryover v current quarter
				if(pos.m[ind1,ind2]>0)
				{
					prob<-c(carryover[[i-1]][ind1,ind2],pos.m[ind1,ind2])/sum(c(carryover[[i-1]][ind1,ind2],pos.m[ind1,ind2]))
				} else
				{
					prob<-c(1,0)
				}

				# if true, selected a person carried over from the previous quarter
				if(sample(1:0,1,prob=prob))
				{
					agent<-neigh(spw=wdist[ind1,neigh.index[[ind1]]],txnew=new.m,ind1=ind1,ind2=ind2,index=neigh.index[[ind1]],site_prob=1)

					if(is.null(agent$neigh_uid))
					{	#agent not assigned
						# print("agent unassigned")
						next
					}
					carryover[[i-1]][ind1,ind2]<-carryover[[i-1]][ind1,ind2]-1
					assigned[[i-1]][[ind1]][assigned[[i-1]][[ind1]]$neigh_uid==agent$neigh_uid,ind2]<-assigned[[i-1]][[ind1]][assigned[[i-1]][[ind1]]$neigh_uid==agent$neigh_uid,ind2]+1
					new_counter<-new_counter-1
					new.m[agent$neigh_uid,ind2]<-new.m[agent$neigh_uid,ind2]-1
					new.m[agent$neigh_uid,"new_tot"]<-new.m[agent$neigh_uid,"new_tot"]-1
					new_tot$new_tot[new_tot$uniqueid==agent$neigh_uid]<-new_tot$new_tot[new_tot$uniqueid==agent$neigh_uid]-1
					next
				}
			}	# end if(carryover[[i-1]][ind1,ind2]>0)

			# selected a person from the current quarter
			agent<-neigh(spw=wdist[ind1,neigh.index[[ind1]]],txnew=new.m,ind1=ind1,ind2=ind2,index=neigh.index[[ind1]],site_prob=1)
			#*** agent assigned or carried over to next quarter***#
			pos.m[agent$home_uid,ind2]<-pos.m[agent$home_uid,ind2]-1
			pos.m[agent$home_uid,"pos_tot"]<-pos.m[agent$home_uid,"pos_tot"]-1
			pos_tot$pos_tot[pos_tot$uniqueid==agent$home_uid]<-pos_tot$pos_tot[pos_tot$uniqueid==agent$home_uid]-1

			if(is.null(agent$neigh_uid))
			{
				carryover[[i]][ind1,ind2]<-carryover[[i]][ind1,ind2]+1
				next
			}
			#*** agent was assigned ***#
			new_counter<-new_counter-1
			new.m[agent$neigh_uid,ind2]<-new.m[agent$neigh_uid,ind2]-1
			new.m[agent$neigh_uid,"new_tot"]<-new.m[agent$neigh_uid,"new_tot"]-1
			new_tot$new_tot[new_tot$uniqueid==agent$neigh_uid]<-new_tot$new_tot[new_tot$uniqueid==agent$neigh_uid]-1

			assigned[[i]][[ind1]][assigned[[i]][[ind1]]$neigh_uid==agent$neigh_uid,ind2]<-assigned[[i]][[ind1]][assigned[[i]][[ind1]]$neigh_uid==agent$neigh_uid,ind2]+1
			#***********************************************#
		}

		if(new_counter==0)
		{
			carryover[[i]][,hts_pos_vars]<-carryover[[i]][,hts_pos_vars]+pos.m[,hts_pos_vars]
			break
		}

	} # end while(pos_counter>0)
	new.unallocated[[i]]<-new.m
	unassigned[[i]]<-pos.m


}	# End for(i in 1:2) loop

ptm2b<- proc.time()
print(((ptm2b-ptm2a)/60)[3])



# ### results######


