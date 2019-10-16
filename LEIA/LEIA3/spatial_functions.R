### spwm: spatial neighbors function #########
#****************************************************************#
spwm<-function(uid,lon,lat)
{
	sites0<-data.frame(uid=uid,lon=lon,lat=lat)
	###Spatial weights matrix####
	n<-nrow(sites0)
	sites2<-cbind(sites0[rep(1:n,each=n),],sites0[rep(1:n,n),])
	names(sites2)<-c(names(sites0),paste(names(sites0),2,sep=""))
	sites2$dist_ft<-km2ft(distGeo(cbind(sites2$lon,sites2$lat),cbind(sites2$lon2,sites2$lat2))/1000)
	sites2$dist_ft[which(sites2$uid==sites2$uid2)]<-0	# set diagonal to zero
	sites2$dist_mi<-ft2miles(sites2$dist_ft)
	w<-matrix(sites2$dist_mi,nrow=n,ncol=n,byrow=T)
	return(list(w=w,site_neigh=sites2))
}
#****************************************************************#


### neigh: nearest neighbors function #########
#****************************************************************#
neigh<-function(spw,txnew,ind1,ind2,index,site_prob=1)
{
	#************************************************#
	# ind1 = site, ind2 = age/sex classification
	#************************************************#
	nneigh<-data.frame(uid=index,new=as.numeric(txnew[index,ind2]),dist=as.numeric(spw))

	#**** remove neighbors without txnew ****#
	nneigh<-nneigh[which(!is.na(nneigh$new)),]
	nneigh<-nneigh[nneigh$new>0,]

	if(nrow(nneigh)==0)
	{
		return(list(home_uid=ind1,neigh_uid=NULL,nneigh=NULL));	# unassigned
	}

	nneigh<-nneigh[order(nneigh$dist,nneigh$uid),]
	nneigh$rank<-rank(nneigh$dist)
	row.names(nneigh)<-NULL

	if(ind1%in%nneigh$uid)
	{	# higher probability of returning to the testing site
		if(site_prob>runif(1))
		{	# home site is the nearest neighbor
			return(list(home_uid=ind1,neigh_uid=ind1,nneigh=nneigh[nneigh$uid==ind1,]))
		}
	}

	#*** agent not assigned to home hospital ***#
	nneigh<-nneigh[!(nneigh$uid%in%ind1),];	#exclude home site
	if(nrow(nneigh)>0)
	{
		nneigh2<-nneigh[nneigh$rank==min(nneigh$rank),]
	} else
	{
		nneigh2<-NULL
	}


	if(is.null(nneigh2))
	{
		return(list(home_uid=ind1,neigh_uid=NULL,nneigh=NULL));	# unassigned
	} else if(nrow(nneigh2)==1)		# a single nearest facility
	{
		return(list(home_uid=ind1,neigh_uid=nneigh2$uid,nneigh=nneigh2))
	} else if(nrow(nneigh2)>1)	# tied nearest facilities
	{	#print("tied nearest facilities")
		neigh_uid=sample(nneigh2$uid,1,prob=nneigh2$new/sum(nneigh2$new))	# randomly select from the tied sites
		return(list(home_uid=ind1,neigh_uid=neigh_uid,nneigh=nneigh2[nneigh2$uid==neigh_uid,]))
	}
}

#****************************************************************#


