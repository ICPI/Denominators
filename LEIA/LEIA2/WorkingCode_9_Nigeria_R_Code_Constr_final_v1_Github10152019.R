
# Working code - YuliangLiu

memory.limit()
memory.limit(16650)
closeAllConnections()

workpath <- "C:\\Users\\onl4\\Documents\\AgentBasedModel2nd90\\FromImran_MohammedMujawar07082019\\Files_Code"
setwd(workpath)
getwd()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Functions used in code ~~~~~~~===============
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Creating basic functions to show top few rows of data
View50 <- function(x){View(x[1:50,])}
View100 <- function(x){View(x[1:100,])}

# Creating the 'not in' function
`%ni%` <- Negate(`%in%`) 

# Load function to install list of packages
ldpkg <- dget("ldpkg.R")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Load (install, if required!) packages ~~~~~~~===============
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#  Load packages, and install them if they are not already installed, before loading
# "plot_ly"
ldpkg( c("leaflet" , 
         "rgdal" , 
         "rgeos", 
         "sp", 
         "RColorBrewer", 
         "tidyverse", 
         "readxl", 
         "htmltools", 
         "htmlwidgets",
         "crosstalk",
         "leaflet.minicharts",
         "haven", 
         "scales",
         "geosphere", 
         "eply",
         "REdaS",
         "janitor",
         "tools",
         "plotly",
         "viridis"
) )

#

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Data Acquisition ~~~~~~~===============
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Getting the Genie dataset file name
glist <- list.files(pattern="MER")

  # Rough data pull to get variable names and assign datatype
  foo <- read_tsv(file=glist, n_max=0,
                  col_names = TRUE)   

foonames <- tolower(names(foo))

rm(foo)

foonames
# class(foonames)


# Creating the vector of column types
colvecx <- as.vector(ifelse(grepl("qtr|cumulative|targets", foonames), "d", "c"))
colvec <- paste(colvecx, collapse = '')


# # Pulling in the data with correct datatype for variables  
msd <- read_tsv(file=glist,
                  col_names = TRUE,
                  col_types = colvec)      # ending if-else for Genie check

names(msd) <- tolower(names(msd))

# # save msd as an object
# save(msd,file = "msd.RData")
load(file = "msd.RData")

# Getting site lat long dat
latlng <- readRDS("Lat_Long_Nigeria_FacilityReport_2018.rds") %>% 
  select(uid, Longitude, Latitude) %>% 
  mutate(uid = as.character(uid))

# # Creating the site key
sitekey <- msd %>%
  select(orgunituid) %>%
  unique() %>%
  mutate(siteid = row_number()) %>%
  rename(uid = orgunituid)

# 
# # Creating data frame with metadat
metadat <- msd %>%
  select(orgunituid, sitename, sitetype, psnu, psnuuid, snu1, snu1uid) %>%
  unique() %>%
  rename(uid = orgunituid)

# # save metadat as an object
# save(metadat,file = "metadat.RData")
load(file = "metadat.RData")
# 
# # Creating data frame for facility lat long data with meta data
geodata <- left_join(metadat, latlng)
# 
# 
# pulling in the PSNU shape files (in R dataset)
psnu_shpx <- readRDS("Nigeria_State_LGA.rds")
# Cleaning up PSNU shape files, taking out NA values
psnu_shp <- psnu_shpx[!is.na(psnu_shpx$code),]


region_shpx <- readRDS("Nigeria_State_shp.rds")
# Cleaning up Region shape files, taking out NA values
region_shp <- region_shpx[!is.na(region_shpx$NAME),]

head(region_shp@data)

# check CRS projection for both- wgs84
proj4string(psnu_shp)
proj4string(region_shp)
# 
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# # ============= Data Restructuring  ~~~~~~~===============
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 
# # Restructuring the dataset to get the Tx dataset and then the choice matrix- 35 snu1
 
linkgx <- msd %>%
  filter(indicator %in% c("HTS_TST_POS","TX_NEW","TX_CURR")) %>%
  gather(period, value, qtr1:qtr4) %>%
  filter(!is.na(value)) %>%
  mutate(agesex = paste(trendsfine, sex, sep="|")) %>%
  # mutate(agesex = paste(AgeAsEntered, sex, sep="|")) %>%
  filter(fiscal_year %in% c("2019")) %>%
  filter(snu1!="_Military Nigeria") %>%
  filter(snu1!=" ") %>%
  # Cleaning up age and sex
  filter(!is.na(sex)) %>%
  filter(!is.na(trendsfine)) %>%
  filter(trendsfine %ni% c("Retired Age Band",
                           "Coarse",
                           "<05")) %>%
  rename(uid=orgunituid)

save(sitekey, geodata, linkgx, file="linkgx08132019.Rdata")
load("linkgx08132019.Rdata")
meta_inf <- geodata %>% 
  select(-c(Longitude,Latitude)) 


# Creating age-sex vector to loop through the ABM for each age-sex category
colnames(linkgx)
agesexvec <- unique(linkgx$agesex)
agesexvec <-agesexvec[-c(21:23)]


link_all <- linkgx %>%
  filter(agesex %in% agesexvec) %>%
  mutate(period = gsub("tr", "", period)) %>%
  mutate(timept = paste(fiscal_year, period, sep="_")) %>%
  select(psnu, psnuuid, uid, sitename, sitetype, indicator, timept, value, agesex) %>%
  group_by_if(is.character) %>%
  summarise(value = sum(value, na.rm=T)) %>%
  ungroup() %>%
  filter(!is.na(value)) %>%
  filter(value!=0) %>%
  group_by_if(is.character) %>%
  summarise(value = sum(value, na.rm=T)) %>%
  spread(indicator, value) %>%
  ungroup()

colnames(link_all)
# rm(msd, metadat)

### ###
# Merging the lat long data onto MER dataset
df_all <- left_join(link_all, latlng)
# Adding on PSNU mean lat long; fortity flatens the spatial data
#psnu polygon has a number lat long associated with it.create a unique id to maintain the mean for a psnu
# Use row numbers to create an id so that each row is unique
psnu_shp@data$id <- rownames(psnu_shp@data)
coord2 <- fortify(psnu_shp, region = 'id')
coord2x <- coord2 %>% group_by(id) %>% summarise(lat=mean(lat), lng=mean(long)) %>% ungroup()

shape_lat_lng <- merge(psnu_shp@data,coord2x, by='id')

psnu_lat_long <- shape_lat_lng %>% select(level5name,uid, lat, lng) %>% 
  rename(psnuuid = uid) %>% 
  rename(psnu=level5name) %>% 
  mutate(psnuuid = as.character(psnuuid))

# Community sites and sites without lat long are give PSNU centroid lat long
df1_all <- left_join(df_all, psnu_lat_long, by="psnuuid") %>% 
  select(-psnu.y) %>% 
  rename(psnu=psnu.x)

# Giving sites without coordinates, PSNU centroid lat long
set.seed(200)
df2x_all <- df1_all %>%
  mutate(lngx = ifelse(is.na(Longitude), jitter(df1_all$lng, factor = 0.001), Longitude),
         latx = ifelse(is.na(Latitude), jitter(df1_all$lat, factor = 0.001), Latitude)) %>%
  mutate(PSNU_lng = ifelse(is.na(Longitude), 1, 0),
         PSNU_lat = ifelse(is.na(Latitude), 1, 0)) %>%
  select(-c(Longitude, Latitude, lng, lat)) %>%
  mutate(lat = round(latx, 6),
         lng = round(lngx, 6)) %>%
  select(-c(latx, lngx))

# Remove facility uid-I7OeRABW6UN, sitename -	Enugu Eastern Medical Hospital
df2x_all <- df2x_all[which(!is.na(df2x_all$lat)),]

# check duplicates if lat, lng for same site - 3272 obs 
df2x_all_check <- df2x_all %>% 
  select(uid,sitename,lat,lng) %>% 
  unique()

# 918 pair duplicates
df2x_all_dup <- df2x_all_check[duplicated(df2x_all_check$uid),]

# add mean(lat), mean(lng) to get rid of duplicate coordinates for each site (total 70 duplicates).
# 3272-918 = 2354 before left_join in the following code
df2x_all_c <- df2x_all %>% 
  select(uid,lat,lng) %>% 
  unique() %>%
  group_by(uid) %>%
  summarise(latm=mean(lat,  na.rm = TRUE), lngm=mean(lng, na.rm = TRUE)) %>%
  ungroup() %>%
  left_join(df2x_all) %>% 
  select(-c(lat, lng)) %>% 
  rename(lat=latm, lng=lngm) %>%
  arrange(uid) 

# check duplicates again - unique - 2354
df2x_all_check_after <- df2x_all_c %>% 
  select(uid,sitename,lat,lng) %>% 
  unique()

q_vec <- c(
  "2019_q1",
  "2019_q2")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####============ AGE-SEX LOOP begins here ==============================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### ### ###
setwd("C:\\Users\\onl4\\Documents\\AgentBasedModel2nd90\\ouput09162019")
getwd()
#Create empty object to store each quarter choice matrix
finallist_facil_dis <- list()
finallist_facdis_ntxhts <- list()

datlst_sav <- list()
dflst_sav <- list()
total_unassignedlst_sav <- list()
total_assignedlst_sav <- list()

# Other rank vectors used
rankvec <- paste("c", formatC(seq(1:100), width=3, flag=0), sep="")

dummyrank <- as.data.frame(rankvec) %>% 
  mutate(value=as.integer(0)) %>% 
  spread(rankvec, value)

dummyfl <- dummyrank[FALSE, ]

# creating function for to parallel process for each age-sex category
for (k in 1:length(agesexvec)){
cat("k = ", k, "\n")
df2x_c <- df2x_all_c %>%   
    filter(agesex %in% agesexvec[k])
linktime <- unique(df2x_c$timept) 
linkt_avail <- Reduce(intersect,list(linktime,q_vec))
qternum <- length(linkt_avail)

if (qternum<2) {
  cat("k = ", k, ": ", qternum, " quarter of data (", linkt_avail,") for ", agesexvec[k], "(excluded)", "\n")
  cat("k = ", k, ": ", qternum, " quarter of data (", linkt_avail,") for ", agesexvec[k], "(excluded)", "\n",
      file="List_AgeSexStrata_Excluded_07262019.txt", append=TRUE)
  next
}

slen <- vector()
for (idx in 1:qternum ) {
  slen[idx] <- grep(linkt_avail[idx],q_vec)
}
slens <- sort(slen)
conti_check <- rle(diff(slens))
if (any(conti_check$values!=1)) {
  cat("k = ", k, ": ", qternum, " quarter of data (", linkt_avail,")", " is not consecutive for ", agesexvec[k], "(excluded)", "\n")
  cat("k = ", k, ": ", qternum, " quarter of data (", linkt_avail,")", " is not consecutive for ", agesexvec[k], "(excluded)", "\n",
      file="List_AgeSexStrata_Excluded_07262019.txt", append=TRUE)
  next
}

df_dummy <- df2x_c[FALSE,FALSE] %>% 
  mutate(TX_NEW = NA,
         TX_CURR = NA,
         HTS_TST_POS = NA)
# df2 <- bind_rows(df2x, df_dummy)
df2 <- bind_rows(df2x_c, df_dummy)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= For facility sites ~~~~~~~===================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Loop through all the quaters data and 
# creates choice matrix for each period, and append into single dataset
datalist = list()   #Create empty object to store each quarter choice matrix

# for (i in 1:length(q_vec)) {
for (i in 1:qternum) {
fl <- df2 %>% 
  filter(sitetype %in% c("Facility")) %>%  
  filter(timept %in% q_vec[slens[i]]) %>% 
  select(sitetype, uid, lat, lng, HTS_TST_POS)


# Swap uid with siteid - ID for timept %in% q_vec[i] - a specific year/quarter
flx <- left_join(fl, sitekey) %>% select(-uid)

name1 <- names(flx)
name2 <- paste(name1, "_2", sep="")

site_1 <- flx
site_2 <- flx
names(site_2) <- name2

site_1x <- site_1 
site_2x <- site_2 %>%  
  select(-HTS_TST_POS_2)


# Creating agents at each facility and community with splay: fl_splay - 759
fl_splay <- site_1x %>% 
  filter(HTS_TST_POS>0) 

fl_splayx <- fl_splay %>% 
  select(sitetype, 
         lat, lng, HTS_TST_POS, siteid) 
  
         
# Doing the cross-merge to get a cartesian product 
cross_df <- merge(x = fl_splayx, y = site_2x, by = NULL)

if(nrow(cross_df)>0){

  # Creating distance values
  fl_maty <- cross_df %>%
    # Remove same site dyads
    filter(siteid != siteid_2) %>%
    # Create the patient data points with variable splay radii
    mutate(distance = distHaversine(cbind(lng, lat), cbind(lng_2, lat_2))) %>%
    mutate(dist_km = distance*0.001) %>%
    arrange(siteid, dist_km) %>%
    #subset for only dydads within top 100 of rank distance
    group_by(siteid) %>%
    mutate(rank_d = row_number()) %>%
    ungroup() %>%
    filter(dist_km<=50) %>%
    # For community sites, first choice for Tx can't be itself
    mutate(crank = paste("c", formatC(rank_d, width=3, flag="0"), sep="")) %>%
    select(sitetype, siteid, lat, lng, HTS_TST_POS, siteid_2, crank) %>%
    spread(crank, siteid_2) %>%
    mutate(c000 = siteid) %>%
    uncount(HTS_TST_POS) %>% 
    mutate(pt_id = row_number())
  
  fl_matx <- bind_rows(fl_maty, dummyfl) %>% 
    select(pt_id,sitetype, siteid, c000, c001:c100, lat, lng) %>% 
    mutate(
      assigned = 0,	
      not_assigned = 1, 
      q_treated	= "",
      fl_treated = NA,
      q_tested = q_vec[i],
      jump	= "")
  
} else{fl_matx <- NULL}  

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= For Community sites ~~~~~~~===================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Setting up loop where each community site is assigned a number of agents 
# based on it's HTS_TST_POS numbers and then distributing the agents
#  randomly over the pSNU it is located in.
## error
cm <- df2 %>% 
  filter(sitetype %in% c("Community") ) %>% 
  filter(timept %in%q_vec[slens[i]]) %>% 
  select(uid, psnuuid, HTS_TST_POS, lat, lng) %>% 
  filter(HTS_TST_POS>0) %>% 
  filter(!is.na(HTS_TST_POS))

if(nrow(cm)>0){
  cmx <- left_join(cm, sitekey) %>% select(-c(uid, psnuuid)) 
  
allcom <-  cmx %>%
  mutate(sitetype = "Community") 


# Doing the cross-merge to get a cartesian product
# # - individual using facility coordinate 58618 obs
cross_df2 <- merge(x = allcom, y = site_2x, by = NULL)

cm_maty <- cross_df2 %>%
  # Create the patient data points with variable splay radii
  mutate(distance = distHaversine(cbind(lng, lat), cbind(lng_2, lat_2))) %>%
  mutate(dist_km = distance*0.001) %>%
  arrange(siteid, dist_km) %>%
  #subset for only dydads within top 100 of rank distance
  group_by(siteid) %>%
  mutate(rank_d = row_number()) %>%
  ungroup() %>%
  filter(dist_km<=50) %>%
  # For community sites, first choice for Tx can't be itself
  # dropping down one rank to make C001 to C00
  mutate(crank = paste("c", formatC(rank_d-1, width=3, flag="0"), sep="")) %>%
  # select(sitetype, siteid, pt_id, lat, lng, HTS_TST_POS, siteid_2, crank) %>%
  select(sitetype, siteid, lat, lng, HTS_TST_POS, siteid_2, crank) %>%
  spread(crank, siteid_2) %>%
  uncount(HTS_TST_POS) %>% 
  mutate(pt_id = row_number())

cm_matx <- bind_rows(cm_maty, dummyfl) %>% 
  select(pt_id, sitetype, siteid, c000, c001:c100, lat, lng) %>% 
  mutate(
    assigned = 0,	
    not_assigned = 1, 
    q_treated	= "",
    fl_treated = NA,	
    jump	= "",
    q_tested = q_vec[slens[i]]
  )

# Stack the facility and community choice matrices 759+225=984 
matx <- bind_rows(fl_matx, cm_matx)
} else {
  matx <- fl_matx}

datalist[[i]] <- matx # add it to your list
}

datlst_sav[[k]] <- datalist


# Stack them all together to create choice matrix for 4 Qs
cmat <- dplyr::bind_rows(datalist) %>% 
  mutate(pt_id = row_number()) %>%   # Give each agent an different ID each quarter
  mutate(agesex = agesexvec[k])

# ===========================================================================
#   FACILITY CONTNIUES
# ===========================================================================

for (z in 1:qternum) {
# Create the corresponding treatment dataset
print(z)

  tx <- df2 %>% 
    filter(sitetype %ni% c("Community")) %>%  
    filter(timept %in% q_vec[slens[z]]) %>% 
    select(uid, timept, TX_NEW)

if (nrow(tx)==0) {next}
  
# Swap uid with siteid
tx1 <- left_join(tx, sitekey) %>% select(-uid)

tx_id <- tx1 %>% 
  rename(fl_id_ray_match_tested = siteid) 

# final treatment dataset
treat_nos <-  tx_id %>% 
  filter(!is.na(fl_id_ray_match_tested)) %>% 
  filter(!is.na(TX_NEW)) %>% 
  filter(TX_NEW!=0) %>% 
  # going wide for each quarter
  # select(fl_id_ray_match_tested, TX_NEW, timept) %>%
  mutate_if(is.numeric, list(~replace(., is.na(.), 0 )))

  treat_nos$txq <- treat_nos$TX_NEW
  qt <- cmat %>% filter(q_tested %in% q_vec[slens[z]])
  if (z==2) {  
    qt<- rbind(qt, qunassigned)
    
  # add remaining q1 capacity to q2
    treat_nos1 <- treat_nos1 %>% 
    rename(txq1=txq,TX_NEW1=TX_NEW, timept1=timept)
  
    treat_nos2 <- left_join(treat_nos,treat_nos1,by="fl_id_ray_match_tested")
    treat_nos2$txq1 <- ifelse(is.na(treat_nos2$txq1),0,treat_nos2$txq1)

    # NA rowsum is NA !
    treat_nos2 <- treat_nos2 %>%
    mutate(txq = rowSums(select(., contains("txq"))))
    treat_nos <- select(treat_nos2,-c(txq1))
  }
  
  if (z==3) {  
    qt<- rbind(qt, qunassigned)
    
    # add remaining q2 capacity to q3
    treat_nos2 <- treat_nos2 %>% 
      rename(txq2=txq,TX_NEW2=TX_NEW, timept2=timept)
    
    treat_nos3 <- left_join(treat_nos,treat_nos2,by="fl_id_ray_match_tested")
    treat_nos3$txq2 <- ifelse(is.na(treat_nos3$txq2),0,treat_nos3$txq2)
    
    # NA rowsum is NA !
    treat_nos3 <- treat_nos3 %>%
      mutate(txq = rowSums(select(., contains("txq"))))
    treat_nos <- select(treat_nos3,-c(txq2))
  }
  
  if (z==4) {
    qt<- rbind(qt, qunassigned)
    
    # add remaining q3 capacity to q4
    treat_nos3 <- treat_nos3 %>% 
      rename(txq3=txq, TX_NEW3=TX_NEW, timept3=timept)
    
    treat_nos4 <- left_join(treat_nos,treat_nos3,by="fl_id_ray_match_tested")
    treat_nos4$txq3 <- ifelse(is.na(treat_nos4$txq3),0,treat_nos4$txq3)
    
    # NA rowsum is NA !
    treat_nos4 <- treat_nos4 %>%
      mutate(txq = rowSums(select(., contains("txq"))))
    treat_nos <- select(treat_nos4,-c(txq3))
  }
  
  n <- nrow(qt)
  set.seed(13)
  qt <- qt[sample(n),]
  
  for (i in 1:n)
  {
    # print(i)
    if(qt$assigned[i]==1) {next}
    if(as.numeric(qt[i,3])==0) {next}
    if(qt[i,3] %in% treat_nos$fl_id_ray_match_tested=="FALSE"){next}
    if(treat_nos$fl_id_ray_match_tested[which(treat_nos$fl_id_ray_match_tested==
                                              as.numeric(qt[i,3]))]==0) {next}
    if(treat_nos$txq[treat_nos$fl_id_ray_match_tested==
                      as.numeric(qt[i,3])]>0) {
      qt$assigned[i]=1 ; qt$q_treated[i]=q_vec[slens[z]];
      qt$not_assigned[i]=0;
      (treat_nos$txq[which(treat_nos$fl_id_ray_match_tested == as.numeric(qt[i,3]))]=
         treat_nos$txq[which(treat_nos$fl_id_ray_match_tested ==as.numeric(qt[i,3]))]-1);
      qt$fl_treated[i]<-as.numeric(qt[i,3]); qt$jump[i]<-1
    }
  }

  for (i in 1:n)
  {
    if(qt$assigned[i]==1) {next}

    for (j in 4:103)
    {
      if(qt$assigned[i]==1) {break}
      if(is.na(qt[i,j])){qt$not_assigned[i]=1; break}
      if(as.numeric(qt[i,j])==0) {qt$not_assigned[i]=1; break}
      if(qt[i,j]%in%treat_nos$fl_id_ray_match_tested=="FALSE"){next}
      if((which(treat_nos$fl_id_ray_match_tested==as.numeric(qt[i,j]))==0)) {next}

      if(treat_nos$txq[which(treat_nos$fl_id_ray_match_tested==as.numeric(qt[i,j]))]>0) {
        qt$assigned[i]=1 ; qt$q_treated[i]=q_vec[slens[z]]; qt$not_assigned[i]=0;
        (treat_nos$txq[which(treat_nos$fl_id_ray_match_tested==as.numeric(qt[i,j]))]=
           treat_nos$txq[which(treat_nos$fl_id_ray_match_tested ==as.numeric(qt[i,j]))]-1);
        qt$fl_treated[i]<-qt[i,j];
        qt$jump[i]<-j

      }
    }
  }
  
  trtnosname <- paste("treat_nos", z, sep="")
  assign(trtnosname, treat_nos)
  
  qasignname <- paste("q", z, "_assigned", sep="")
  qassigned<-subset(qt, qt$assigned==1)
  assign(qasignname, qassigned)
  
  qunasignname <- paste("q", z, "_unassigned", sep="")
  qunassigned<-subset(qt, qt$assigned==0)
  assign(qunasignname, qunassigned)
  
  if (z>=2) {
    qdropoutname <- paste("q", z-1, "_dropout", sep="")
    qdropout<-subset(qunassigned, qunassigned$q_tested==q_vec[z-1])
    assign(qdropoutname, qdropout)
    
    qunassigned<-subset(qunassigned, qunassigned$q_tested!=q_vec[z-1])
    assign(qunasignname, qunassigned)
  }
}

# unassigned --------------------------------------------------------------
dropout_list <-  list()
assigned_list <- list()
if(qternum > 1) {
  for (j in 1:(qternum-1)) {
  qdropoutname <- paste("q", j, "_dropout", sep="")
  dropout_list [[j]] <- get(qdropoutname)
  }
  total_dropout <- dplyr::bind_rows(dropout_list)
  qunassignname <- paste("q", j+1, "_unassigned", sep="")
  assign("remaining_unassinged", get(qunassignname))
  total_unassigned <- rbind(total_dropout,remaining_unassinged) %>% 
    select(pt_id, 
           siteid, 
           lat,         
           lng,          
           assigned,    
           not_assigned, 
           q_treated,  
           fl_treated,
           q_tested,
           jump,
           agesex,      
           sitetype)
  total_unassignedlst_sav[[k]] <- total_unassigned
  
  for (w in 1:qternum) {
    qassignname <- paste("q", w, "_assigned", sep="")
    assigned_list[[w]] <- get(qassignname)
  }
  total_assigned<- dplyr::bind_rows(assigned_list) %>% 
    select(pt_id, 
           siteid, 
           lat,         
           lng,          
           assigned,    
           not_assigned, 
           q_treated,  
           fl_treated,
           q_tested,
           jump,
           agesex,      
           sitetype)
  total_assignedlst_sav[[k]] <- total_assigned
}


# Creating site-level dataset with updated coverage numbers
link_df <- total_assigned %>%
  mutate(fl_treated = as.numeric(fl_treated)) %>%
  group_by(siteid, lat, lng, fl_treated) %>%
  summarise(treated = sum(assigned, na.rm=T)) %>%
  ungroup() %>%
  mutate(linktype = if_else(siteid==fl_treated, "intr", "extr")) %>%
  group_by(siteid, lat, lng, fl_treated, linktype) %>%
  summarise(treated = sum(treated, na.rm=T)) %>%
  ungroup() %>%
  spread(linktype, treated) %>%
  mutate_if(is.numeric, list(~replace(., is.na(.), 0 ))) %>%
  mutate(tot_tx = extr + intr)

unlink_df <- total_unassigned %>% group_by(siteid, lat, lng,) %>%
  summarise(untreated = sum(not_assigned, na.rm=T)) %>%
  ungroup()

alllink <- bind_rows(link_df, unlink_df)

# Create corresponding APR treatment and testing datasets
txhts <- df2 %>%
  filter(timept %in% q_vec) %>%
  select(uid, lat, lng, TX_NEW, HTS_TST_POS) %>%
  group_by(uid,lat, lng) %>%
  summarize_all(list(~sum), na.rm=T) %>%
  ungroup()

txhtsx <- left_join(txhts, sitekey) %>% select(-uid)


all_df <- bind_rows(alllink, txhtsx)

# Adding back the orguniuid
alllink_f <- left_join(all_df, sitekey)
alllink_f_ntxhts <- left_join(alllink, sitekey)


f_df <- left_join(alllink_f, meta_inf)
f_df_ntxhts <- left_join(alllink_f_ntxhts, meta_inf) 

# adding info for the treatment site
names(f_df)  <-   c("mod_tst_id",
                    "lat_tst",
                    "lng_tst",
                     "mod_rx_id",
                     "ext_rx",
                     "int_rx",
                     "tot_rx",
                     "untreated",
                     "tx_new",
                     "hts_pos",
                     "uid_tst",
                     "sitename_tst",
                     "sitetype_tst",
                     "psnu_tst",
                     "psnuuid_tst",
                     "snu1_tst",
                     "snu1uid_tst")

names(f_df_ntxhts)  <-   c("mod_tst_id",
                    "lat_tst",
                    "lng_tst",
                    "mod_rx_id",
                    "ext_rx",
                    "int_rx",
                    "tot_rx",
                    "untreated",
                    "uid_tst",
                    "sitename_tst",
                    "sitetype_tst",
                    "psnu_tst",
                    "psnuuid_tst",
                    "snu1_tst",
                    "snu1uid_tst")


 
sitekey2 <- sitekey %>% rename(uid_rx = uid,
                               mod_rx_id = siteid)  

meta_inf2 <- meta_inf %>% 
  rename(uid_rx = uid, 
         sitename_rx = sitename, 
         sitetype_rx = sitetype, 
         psnu_rx = psnu, 
         psnuuid_rx = psnuuid,
         snu1_rx = snu1, 
         snu1uid_rx = snu1uid) 

    fdf <- left_join(f_df, sitekey2)
    fdf_ntxhts <- left_join(f_df_ntxhts, sitekey2)

    fdfx <- left_join(fdf, meta_inf2) 
    fdfx_ntxhts <- left_join(fdf_ntxhts, meta_inf2) 

    cord4rx <- df2x_all_check_after %>% 
      select(-c(sitename)) %>% 
      rename(uid_rx = uid,
             lat_rx = lat,
             lng_rx = lng)

    fdfxc <- left_join(fdfx, cord4rx) %>%
      mutate(agesex = agesexvec[k])
    fdfxc_ntxhts <- left_join(fdfx_ntxhts, cord4rx) %>% 
      mutate(agesex = agesexvec[k])

    finallist_facil_dis[[k]] <- fdfxc
    finallist_facdis_ntxhts[[k]] <- fdfxc_ntxhts
 }


# save final list as an object

save(finallist_facil_dis, file = "finallist_facil_dis.RData")
# load("finallist_facil_dis.RData")
save(finallist_facdis_ntxhts, file = "finallist_facdis_ntxhts.RData")
# load("finallist_facdis_ntxhts.RData")
save(dflst_sav, file = "dflst_sav.RData")
# load("dflst_sav.RData")
save(datlst_sav, file = "datlst_sav.RData")
# load("datlst_sav.RData")
save(total_unassignedlst_sav, file = "total_unassignedlst_sav.RData")
# load("total_unassignedlst_sav.RData")
save(total_assignedlst_sav, file = "total_assignedlst_sav.RData")
# load("total_assignedlst_sav.RData")

Finaldf <- dplyr::bind_rows(finallist_facil_dis)
Finaldf_facdis_ntxhts <- dplyr::bind_rows(finallist_facdis_ntxhts)

Finaldf_dflst_sav <- dplyr::bind_rows(dflst_sav)
Finaldf_datlst_sav <- do.call(dplyr::bind_rows, datlst_sav)
Finaldf_total_unassignedlst_sav <- dplyr::bind_rows(total_unassignedlst_sav)
Finaldf_total_assignedlst_sav <- dplyr::bind_rows(total_assignedlst_sav)

dx <- as.character(format(Sys.time(), "%Y %b %d"))
t <- as.character(format(Sys.time(), "%H %M %S" ))
tm <- str_replace_all(t, "[ ]", "_")
dt <- str_replace_all(dx, "[ ]", "")

write.csv(Finaldf, paste("Output/", dt, "_AdjLinkage_final_facilDist_", tm, ".csv", sep=""), na="", row.names = F)
write.csv(Finaldf_facdis_ntxhts, paste("Output/", dt, "_AdjLinkage_final_facdis_ntxhts_", tm, ".csv", sep=""), na="", row.names = F)
write.csv(Finaldf_dflst_sav, paste("Output/", dt, "_AdjLinkage_dflst_sav_", tm, ".csv", sep=""), na="", row.names = F)
write.csv(Finaldf_datlst_sav, paste("Output/", dt, "_AdjLinkage_datlst_sav_", tm, ".csv", sep=""), na="", row.names = F)

for (ck in 1:12) {
  Finaldf_total_unassignedlst_sav[,ck] <- Finaldf_total_unassignedlst_sav[,ck] %>% unlist()
  cat(ck, " - ", colnames(Finaldf_total_unassignedlst_sav)[ck], " - ", typeof(Finaldf_total_unassignedlst_sav[,ck]), "\n",
      file="typeofvariablesIn_Finaldf_total_unassignedlst_sav_07262019_unlist.txt", append=TRUE)
}

write.csv(Finaldf_total_unassignedlst_sav, paste("Output/", dt, "_AdjLinkage_total_unassignedlst_sav_", tm, ".csv", sep=""), na="", row.names = F)

for (ck in 1:12) {
  Finaldf_total_assignedlst_sav[,ck] <- Finaldf_total_assignedlst_sav[,ck] %>% unlist()
  cat(ck, " - ", colnames(Finaldf_total_assignedlst_sav)[ck], " - ", typeof(Finaldf_total_assignedlst_sav[,ck]), "\n",
      file="typeofvariablesIn_Finaldf_total_assignedlst_sav_07302019_unlist.txt", append=TRUE)
}

write.csv(Finaldf_total_assignedlst_sav, paste("Output/", dt, "_AdjLinkage_total_assignedlst_sav_", tm, ".csv", sep=""), na="", row.names = F)


#########################################################################
#########################################################################
## Visulization
## choose data folder
# 2019 Q1 Q2
datapath <- "C:\\Users\\onl4\\Documents\\AgentBasedModel2nd90\\ouput08132019"
setwd(datapath)

# prepare data
load("finallist_facil_dis.RData")
load("total_unassignedlst_sav.RData")
load("total_assignedlst_sav.RData")

Finaldf <- dplyr::bind_rows(finallist_facil_dis)
Finaldf_total_unassignedlst_sav <- dplyr::bind_rows(total_unassignedlst_sav)
Finaldf_total_assignedlst_sav <- dplyr::bind_rows(total_assignedlst_sav)

# 
Finaldf_total_agent <- bind_rows(Finaldf_total_unassignedlst_sav,Finaldf_total_assignedlst_sav) %>% 
  group_by(siteid, agesex, sitetype) %>%
  summarise(sa_assigned = sum(assigned), sa_unassigned = sum(not_assigned), 
            sa_lat=mean(lat), sa_lng=mean(lng), na.rm=T) %>%
  ungroup() %>%
  select(siteid, sa_lat, sa_lng, sa_assigned, sa_unassigned, agesex, sitetype) %>% 
  rowwise() %>% 
  mutate(sa_unassign_prop=sa_unassigned/sum(sa_assigned, sa_unassigned)) %>% 
  ungroup()  %>% 
  mutate_if(is.numeric, list(~replace(., is.na(.), 0 )))

setwd(datapath)
saveRDS(Finaldf_total_agent, "Finaldf_total_agent2019q1q2.rds")
Finaldf_total_agent <- readRDS("Finaldf_total_agent2019q1q2.rds")

# Merge for sitename
setwd(workpath)
load(file = "metadat.RData")
load("linkgx08132019.Rdata")

skmeta_dat <- inner_join(sitekey,metadat) %>%
  select(siteid,sitename)
Finaldf_total_agent_snm <- left_join(Finaldf_total_agent,skmeta_dat)

# Merge for total unassigned and total assigned for each site in age sex strata data
setwd(datapath)
Finaldf_total_agent_allag <- readRDS("Finaldf_total_agent_allag2019q1q2.rds")

Finaldf_total_agent_snm_tassign <- Finaldf_total_agent_allag %>%
  select(siteid, sa_assigned, sa_unassigned, sa_unassign_prop) %>%
  rename(tot_assigned=sa_assigned, 
         tot_unassigned=sa_unassigned, 
         tot_sa_unassign_prop=sa_unassign_prop) %>%
  right_join(Finaldf_total_agent_snm)  %>%
  select(siteid, sitename, sitetype, sa_lat, sa_lng, sa_assigned, sa_unassigned, 
         sa_unassign_prop, tot_assigned, tot_unassigned, tot_sa_unassign_prop, agesex)

saveRDS(Finaldf_total_agent_snm_tassign, "Finaldf_total_agent_snm_tassign2019q1q2.rds")
Finaldf_total_agent_snm_tassign <- readRDS("Finaldf_total_agent_snm_tassign2019q1q2.rds")

colnames(Finaldf_total_agent_snm_tassign)

as_vec <- unique(Finaldf_total_agent$agesex) # class(as_vec)

### ###
# calculate adjusted and proxy linkage values for all SNU1 by each agesex strata
snu_link <- Finaldf %>% 
  select(snu1_tst, snu1uid_tst,
         tot_rx, tx_new,	hts_pos) %>% 
  group_by(snu1_tst, snu1uid_tst) %>% 
  summarize_all(list(~sum), na.rm=T) %>% 
  mutate(prox_link = round((tx_new/hts_pos)*100,0),
         abm_link = round((tot_rx/hts_pos)*100,0))

snu_link$snu1 <- factor(snu_link$snu1_tst, levels = snu_link$snu1_tst[order(snu_link$snu1_tst)])

# dumbell plot for all SNU1 by each agesex strata

f <- list(
  family = "Arial",
  size = 18,
  color = "#7f7f7f"
)
x <- list(
  title = "% linkage",
  titlefont = f
)
y <- list(
  title = "SNU1",
  titlefont = f
)

p <- plot_ly(snu_link, color = I("gray80")) %>%
  add_segments(x = ~prox_link, xend = ~abm_link, y = ~snu1, yend = ~snu1, showlegend = FALSE) %>%
  add_markers(x = ~prox_link, y = ~snu1, name = "Proxy", color = I("#B5B867")) %>%
  add_markers(x = ~abm_link, y = ~snu1, name = "ABM", color = I("#335B8E")) %>% 
  layout(xaxis = x, yaxis = y)

# calculate adjusted and proxy linkage values for all agesex strata by each SNU1
snu1vec <- unique(Finaldf$snu1_tst)
snu_link_ag <- Finaldf %>% 
  filter(snu1_tst %in% snu1vec[1]) %>%
  select(snu1_tst, snu1uid_tst, agesex,
         tot_rx, tx_new,	hts_pos) %>% 
  group_by(snu1_tst, snu1uid_tst, agesex) %>% 
  summarize_all(list(~sum), na.rm=T) %>% 
  mutate(prox_link = round((tx_new/hts_pos)*100,0),
         abm_link = round((tot_rx/hts_pos)*100,0))

snu_link_ag$agesex <- factor(snu_link_ag$agesex, levels = snu_link_ag$agesex[order(snu_link_ag$agesex)])

# dumbell plot for all agesex strata by each SNU1
library(plotly)

f <- list(
  family = "Arial",
  size = 18,
  color = "#7f7f7f"
)
x <- list(
  title = "% linkage",
  titlefont = f
)
y <- list(
  title = "Age-Gender",
  titlefont = f
)

p <- plot_ly(snu_link_ag, color = I("gray80")) %>%
  add_segments(x = ~prox_link, xend = ~abm_link, y = ~agesex, yend = ~agesex, showlegend = FALSE) %>%
  add_markers(x = ~prox_link, y = ~agesex, name = "Proxy", color = I("#B5B867")) %>%
  add_markers(x = ~abm_link, y = ~agesex, name = "ABM", color = I("#335B8E")) %>% 
  layout(xaxis = x, yaxis = y) %>% 
  layout(title=paste("SNU1 - ", snu1vec[1]))
p

# loop for each snu1
path <- "C:\\Users\\onl4\\Documents\\AgentBasedModel2nd90\\FromImran_MohammedMujawar07082019\\Files_Code"
graphpath <- file.path(path,"Graph")
dumbell_path <- file.path(graphpath, "Dumbell_Plot")
setwd(dumbell_path)
# setwd("\\cdc.gov\\private\\M336\\onl4\AgentBasedModel2nd90\\FromImran_MohammedMujawar07082019\\Files_Code\\Graph")
getwd()

library(plotly)
library(htmlwidgets)
f <- list(
  family = "Arial",
  size = 18,
  color = "#7f7f7f"
)
x <- list(
  title = "% linkage",
  titlefont = f
)
y <- list(
  title = "Age-Gender",
  titlefont = f
)
snu1vec <- unique(Finaldf$snu1_tst)
for (i in 1:length(snu1vec)){
    snuv_i <- str_replace_all(snu1vec[i], "[ ]", "_")
    snu_link_ag <- Finaldf %>% 
    filter(snu1_tst %in% snu1vec[i]) %>%
    select(snu1_tst, snu1uid_tst, agesex,
           tot_rx, tx_new,	hts_pos) %>% 
    group_by(snu1_tst, snu1uid_tst, agesex) %>% 
    summarize_all(list(~sum), na.rm=T) %>% 
    mutate(prox_link = round((tx_new/hts_pos)*100,0),
           abm_link = round((tot_rx/hts_pos)*100,0))

    p <- plot_ly(snu_link_ag, color = I("gray80")) %>%
    add_segments(x = ~prox_link, xend = ~abm_link, y = ~agesex, yend = ~agesex, showlegend = FALSE) %>%
    add_markers(x = ~prox_link, y = ~agesex, name = "Proxy", color = I("#B5B867")) %>%
    add_markers(x = ~abm_link, y = ~agesex, name = "ABM", color = I("#335B8E")) %>% 
    layout(xaxis = x, yaxis = y) %>% 
    layout(title=paste("SNU1 - ", snu1vec[i]))
    
    out_file = paste("SNU1_",snuv_i,"_agesex.html",sep="")
    saveWidget(p, out_file)
    # plotly_IMAGE(p, format = "png", out_file = paste("SNU1_",snu1vec[i],"_agesex.png",sep=""))
}

# dumbell plot for all age-sex strata
# aggregate Finaldf across all age-sex strata
# calculate adjusted and proxy linkage values for all agesex strata by each SNU1
library(plotly)
library(htmlwidgets)
f <- list(
  family = "Arial",
  size = 18,
  color = "#7f7f7f"
)
x <- list(
  title = "% linkage",
  titlefont = f
)
y <- list(
  title = "Age-Gender",
  titlefont = f
)


  snu_link_all <- Finaldf %>% 
    select(agesex, 
           tot_rx, tx_new, hts_pos) %>% 
    group_by(agesex) %>% 
    summarize_all(list(~sum), na.rm=T) %>% 
    mutate(prox_link = round((tx_new/hts_pos)*100,0),
           abm_link = round((tot_rx/hts_pos)*100,0))
 
  snu_link_all$agesex <- factor(snu_link_all$agesex, levels = snu_link_all$agesex[order(snu_link_all$agesex)])
  snu_link_all <- snu_link_all[-1,] # remove the agesex strata:<01|Unknown Sex-prox_link=4364
  p <- plot_ly(snu_link_all, color = I("gray80")) %>%
    add_segments(x = ~prox_link, xend = ~abm_link, y = ~agesex, yend = ~agesex, showlegend = FALSE) %>%
    add_markers(x = ~prox_link, y = ~agesex, name = "Proxy", color = I("#B5B867")) %>%
    add_markers(x = ~abm_link, y = ~agesex, name = "ABM", color = I("#335B8E")) %>% 
    layout(xaxis = x, yaxis = y) %>% 
    layout(title=paste("ALL SNU1s "))
  
  out_file = paste("All_SNU1_","by_agesex.html",sep="")
  saveWidget(p, out_file)

  ### ### ###

# Map
# Calculate adjusted and proxy linkage values at PSNU level
psnulink <- Finaldf %>% 
  select(psnu_tst, psnuuid_tst,
         tot_rx, tx_new,	hts_pos) %>% 
  group_by(psnu_tst, psnuuid_tst) %>% 
  summarize_all(list(~sum), na.rm=T) %>% 
  ungroup() %>% 
  mutate(prox_link = round((tx_new/hts_pos)*100,0),
         abm_link = round((tot_rx/hts_pos)*100,0)) %>% 
  # Remove NA and NaNs
  mutate_if(is.double, list(~replace(., is.na(.), NA_real_))) %>% 
  mutate_if(is.double, list(~replace(., is.nan(.), NA_real_))) %>% 
  mutate_if(is.double, list(~replace(., .>999999999, NA_real_)))

# pulling in the PSNU shape files (in R dataset) and
# Cleaning up PSNU shape files, taking out NA values
setwd(workpath)
psnu_shpx <- readRDS("Nigeria_State_LGA.rds")
psnu_shp <- psnu_shpx[!is.na(psnu_shpx$uid),]

# Read and clean up Region shape files, taking out NA values
region_shpx <- readRDS("Nigeria_State_shp.rds")
region_shp <- region_shpx[!is.na(region_shpx$ID),]
# View(region_shp@data)
# colnames(region_shp@data)

shape_df <- merge(psnu_shp, psnulink, by.x = "uid", by.y = "psnuuid_tst")

n_psnu_abm_link <- length(unique(shape_df$abm_link))
c_abm <- colorRampPalette(c('#cc5234', '#b5b867', '#6ca18f'))(n_psnu_abm_link)
pal_abm <- colorNumeric(palette = c_abm, domain = shape_df$abm_link)

n_psnu_prox_link <- length(unique(shape_df$prox_link))
c_prox <- colorRampPalette(c('#cc5234', '#b5b867', '#6ca18f'))(n_psnu_prox_link)
pal_prox <- colorNumeric(palette = c_prox, domain = shape_df$prox_link)

# Visualizing PSNU-level ABM linkage
# ABM
psnu_map_abm <- leaflet(data=shape_df) %>%
  addPolygons(fillColor = ~pal_abm(abm_link), 
              color='white', weight=1, opacity=0.9, fillOpacity = 0.9) %>% 
  addPolygons(data=region_shp,fillColor = "grey", 
              color='black', weight=1.5, opacity=0.4, fillOpacity = 0.01) %>% 
  addLegend(pal = pal_abm, values = ~abm_link,
            labFormat = labelFormat(suffix = '%', between = '% - '),
            opacity = 0.9, title = 'ABM Linkage',position = "bottomright") %>%
  addScaleBar()

# PROX
psnu_map_prox <- leaflet(data=shape_df) %>%
  addPolygons(fillColor = ~pal_prox(prox_link), 
              color='white', weight=1, opacity=0.5, fillOpacity = 0.9) %>% 
  addLegend(pal = pal_prox, values = ~prox_link,
            labFormat = labelFormat(suffix = '%', between = '% - '),
            opacity = 0.9, title = 'Proxy Linkage',position = "bottomright") %>%
  addScaleBar()

# map label 
shape_df$psnu_label <- if_else(is.na(shape_df$psnu_tst), "", 
                               if_else(is.na(shape_df$abm_link), shape_df$psnu_tst, 
                                       paste(shape_df$psnu_tst, ": ", shape_df$abm_link, "%")))

# Only unassigned agents, by community and facility
agentmapf <- total_agent

# Checking placement of sites in appropriate
colfn2 <- colorRampPalette(c('#cc5236', '#335b8e'))(2)
pal2 <- colorFactor(palette = colfn2, domain = agentmapf$sitetype)

state_popup <- paste0("<strong>SNU1: </strong>", region_shp$NAME)
psnu_map_abm_ua <- leaflet(data=shape_df) %>%
  addPolygons(fillColor = ~pal_abm(abm_link), 
              color='white', weight=1, opacity=0.9, fillOpacity = 0.9) %>% 
  addPolygons(data=region_shp,fillColor = "grey", 
              color='black', weight=1.5, opacity=0.4, fillOpacity = 0.01, popup = state_popup) %>% 
  addLegend(pal = pal_abm, values = ~abm_link,
            labFormat = labelFormat(suffix = '%', between = '% - '),
            opacity = 0.9, title = 'ABM Linkage', position = "bottomright") %>%
  addCircleMarkers(data=agentmapf, lng=~sa_lng, lat=~sa_lat, radius=~sqrt(sa_unassigned)*2, stroke = T,
                   fillOpacity=1,label=~paste(siteid, sep="_"), color=~pal2(sitetype),
                   labelOptions = labelOptions(noHide = F, direction = 'topright', 
                                               style = list("color" = "blue"))) %>% 
  addLegend(data=agentmapf, pal = pal2, values = ~sitetype, group = "circles", position = "bottomright") %>%  
  addScaleBar()

psnu_map_abm_uap <- leaflet(data=shape_df) %>%
  # addTiles() %>%
  addPolygons(fillColor = ~pal_abm(abm_link), 
              color='white', weight=1, opacity=0.9, fillOpacity = 0.9) %>% 
  addPolygons(data=region_shp,fillColor = "grey", 
              color='black', weight=1.5, opacity=0.4, fillOpacity = 0.01, popup = state_popup) %>% 
  addLegend(pal = pal_abm, values = ~abm_link,
            labFormat = labelFormat(suffix = '%', between = '% - '),
            opacity = 0.9, title = 'ABM Linkage', position = "bottomright") %>%
  addCircleMarkers(data=agentmapf, lng=~sa_lng, lat=~sa_lat, radius=~sqrt(sa_unassign_prop)*5, stroke = T,
                   fillOpacity=1,label=~paste(siteid, sep="_"), color=~pal2(sitetype),
                   labelOptions = labelOptions(noHide = F, direction = 'topright', 
                                               style = list("color" = "blue"))) %>% 
  addLegend(data=agentmapf, pal = pal2, values = ~sitetype, group = "circles", position = "bottomright") %>%  
  addScaleBar()

# save map
# library(htmltools)
setwd("C:\\Users\\onl4\\Documents\\AgentBasedModel2nd90\\FromImran_MohammedMujawar07082019\\Files_Code\\Graph\\Map08032019")
getwd()

library(htmlwidgets)
saveWidget(psnu_map_abm_uap, file="test.html")

# loop and save all maps: 2406 sites
library(htmlwidgets)
rr <- HTML('<title> TITLE </title>')
setwd(datapath)
map_path <- file.path(datapath,"/Graph/Map08152019")
as_vec <- unique(Finaldf_total_agent_snm_tassign$agesex) # class(as_vec)
psnu_popup <- paste0("<strong>PSNU: </strong>", psnu_shp$Name)

for (i in 1:length(as_vec)){
  asvec_1 <- str_replace_all(as_vec[i], "[|]", "_")
  asvec_1 <- str_replace_all(asvec_1, "[<]", "less_than_")
  asvec_i <- str_replace_all(asvec_1 , "[ ]", "")
  datnm <- paste("total_ag_snm_tass_",asvec_i,sep = '')
  ag <- subset(Finaldf_total_agent_snm_tassign,agesex==as_vec[i]) %>% arrange(siteid)
  assign(datnm,ag)
  agentmapf <- ag 
  if (length(unique(agentmapf$sitetype))==2) {
    colfn2 <- colorRampPalette(c('#cc5236', '#335b8e'))(2)
    pal2 <- colorFactor(palette = colfn2, domain = agentmapf$sitetype)
  } else {
    colfn2 <- colorRampPalette(c('#335b8e'))(1)
    pal2 <- colorFactor(palette = colfn2, domain = agentmapf$sitetype)
  }

  popuplabel1 <- paste0(
    '
    <b>', agentmapf$sitename,'</b><br><br>
    <b>Unassigned</b><br>
    Age Sex (', agentmapf$agesex, ') : ', agentmapf$sa_unassigned,'<br>
    Total : ', agentmapf$tot_unassigned,'<br>
    <b>Assigned</b><br>
    Age Sex (', agentmapf$agesex, ') : ', agentmapf$sa_assigned,'<br>
    Total : ', agentmapf$tot_assigned,'<br><br>
    <b>Site ID : ', agentmapf$siteid,'<b><br><br>
    ')
  psnu_map_abm_ua <- leaflet(data=shape_df) %>%
    addTiles() %>%
    addPolygons(data=region_shp,fillColor = "grey", 
                color='black', weight=1.8, opacity=1, fillOpacity = 0.01) %>% 
    addPolygons(fillColor = ~pal_abm(abm_link), 
                color='white', weight=1, opacity=0.6, fillOpacity = 0.55, popup = psnu_popup) %>%
    addLegend(pal = pal_abm, values = ~abm_link,
              labFormat = labelFormat(suffix = '%', between = '% - '),
              opacity = 0.6, title = 'ABM Linkage', position = "bottomright") %>%
    addCircleMarkers(data=agentmapf, lng=~sa_lng, lat=~sa_lat, radius=~sqrt(sa_unassigned)*2, stroke = F,
                     opacity=1, fillOpacity=1, popup=popuplabel1, color=~pal2(sitetype),
                     popupOptions = popupOptions(closeButton = T)) %>% 
    addLegend(data=agentmapf, opacity=1, pal = pal2, values = ~sitetype, group = "circles", position = "bottomright") %>%  
    addScaleBar() %>%
    addControl(rr, position = "topleft")
  
  # table popup for unassigned proportion map
  popuplabel2 <- paste0(
    '
    <b>', agentmapf$sitename,'</b><br><br>
    <b>Proportion of Unassigned</b><br>
    Age Sex (', agentmapf$agesex, ') : ', round(agentmapf$sa_unassign_prop*100,1),'%<br>
    <b>Unassigned</b><br>
    Age Sex (', agentmapf$agesex, ') : ', agentmapf$sa_unassigned,'<br>
    Total : ', agentmapf$tot_unassigned,'<br>
    <b>Assigned</b><br>
    Age Sex (', agentmapf$agesex, ') : ', agentmapf$sa_assigned,'<br><br>
    <b>Site ID : ', agentmapf$siteid,'<b><br><br>
    ')
  psnu_map_abm_uap <- leaflet(data=shape_df) %>%
    addTiles() %>%
    addPolygons(data=region_shp,fillColor = "grey",
                color='black', weight=1.8, opacity=1, fillOpacity = 0.01) %>%
    addPolygons(fillColor = ~pal_abm(abm_link),
                color='white', weight=1, opacity=0.6, fillOpacity = 0.55, popup = psnu_popup) %>%
    addLegend(pal = pal_abm, values = ~abm_link,
              labFormat = labelFormat(suffix = '%', between = '% - '),
              opacity = 0.6, title = 'ABM Linkage', position = "bottomright") %>%
    addCircleMarkers(data=agentmapf, lng=~sa_lng, lat=~sa_lat, radius=~sqrt(sa_unassign_prop)*5, stroke = F,
                     opacity=1, fillOpacity=1,popup=popuplabel2, color=~pal2(sitetype),
                     popupOptions = popupOptions(closeButton = T)) %>% 
    addLegend(data=agentmapf, opacity=1, pal = pal2, values = ~sitetype, group = "circles", position = "bottomright") %>%  
    addScaleBar() %>%
    addControl(rr, position = "topleft")
  
  mapnm_ua <- paste("psnu_abm_pop_ua2019q1q2_", asvec_i, ".html",sep="")
  mapnm_uap <- paste("psnu_abm_pop_uaprop2019q1q2_",asvec_i, ".html",sep="")
  saveWidget(psnu_map_abm_ua, file.path(map_path,mapnm_ua))
  saveWidget(psnu_map_abm_uap, file.path(map_path,mapnm_uap))
}

################################################################################################################
################################################################################################################

# Grouped layer control
as_vec <- unique(Finaldf_total_agent_snm_tassign$agesex)
ag <- list()
popuplabel_a <- list()
popuplabel_b <- list()
for (i in 1:length(as_vec)){
  ag[[i]] <- subset(Finaldf_total_agent_snm_tassign,agesex==as_vec[i]) %>% arrange(siteid)
  popuplabel_a[[i]] <- paste0(
    '
    <b>', ag[[i]]$sitename,'</b><br><br>
    <b>Unassigned</b><br>
    Age Sex (', ag[[i]]$agesex, ') : ', ag[[i]]$sa_unassigned,'<br>
    Total : ', ag[[i]]$tot_unassigned,'<br>
    <b>Assigned</b><br>
    Age Sex (', ag[[i]]$agesex, ') : ', ag[[i]]$sa_assigned,'<br>
    Total : ', ag[[i]]$tot_assigned,'<br><br>
    <b>Site ID : ', ag[[i]]$siteid,'<b><br><br>
    ')
  # table popup for unassigned proportion map
  popuplabel_b[[i]] <- paste0(
    '
    <b>', ag[[i]]$sitename,'</b><br><br>
    <b>Proportion of Unassigned</b><br>
    Age Sex (', ag[[i]]$agesex, ') : ', round(ag[[i]]$sa_unassign_prop*100,1),'%<br>
    <b>Unassigned</b><br>
    Age Sex (', ag[[i]]$agesex, ') : ',ag[[i]]$sa_unassigned,'<br>
    Total : ', ag[[i]]$tot_unassigned,'<br>
    <b>Assigned</b><br>
    Age Sex (', ag[[i]]$agesex, ') : ', ag[[i]]$sa_assigned,'<br><br>
    <b>Site ID : ', ag[[i]]$siteid,'<b><br><br>
    ')
}

    colfn2 <- colorRampPalette(c('#cc5236', '#335b8e'))(2)
    pal2 <- colorFactor(palette = colfn2, domain = ag[[1]]$sitetype)
    colfn1 <- colorRampPalette(c('#335b8e'))(1)

  psnu_map_abm_ua <- leaflet(data=shape_df) %>%
    addTiles() %>%
    addPolygons(data=region_shp,fillColor = "grey", 
                color='black', weight=1.8, opacity=1, fillOpacity = 0.01) %>% 
    addPolygons(fillColor = ~pal_abm(abm_link), 
                color='white', weight=1, opacity=0.6, fillOpacity = 0.55, popup = psnu_popup) %>%
    addLegend(pal = pal_abm, values = ~abm_link,
              labFormat = labelFormat(suffix = '%', between = '% - '),
              opacity = 0.6, title = 'ABM Linkage', position = "bottomright") %>%
    addCircleMarkers(data=ag[[1]], lng=~sa_lng, lat=~sa_lat, radius=~sqrt(sa_unassigned)*2, stroke = F,
                     opacity=1, fillOpacity=1,  
                     group=as_vec[1], 
                     popup=popuplabel_a[[1]], popupOptions = popupOptions(closeButton = T),
                     color=~colorFactor(palette = colfn2, domain = ag[[1]]$sitetype)(sitetype)) %>% 
                     addCircleMarkers(data=ag[[2]], lng=~sa_lng, lat=~sa_lat, radius=~sqrt(sa_unassigned)*2, stroke = F,
                     opacity=1, fillOpacity=1,  
                     group=as_vec[2], 
                     popup=popuplabel_a[[2]], popupOptions = popupOptions(closeButton = T),
                     color=~colorFactor(palette = colfn2, domain = ag[[2]]$sitetype)(sitetype)) %>%
    addCircleMarkers(data=ag[[3]], lng=~sa_lng, lat=~sa_lat, radius=~sqrt(sa_unassigned)*2, stroke = F,
                     opacity=1, fillOpacity=1,  
                     group=as_vec[3], 
                     popup=popuplabel_a[[3]], popupOptions = popupOptions(closeButton = T),
                     color=~colorFactor(palette = colfn2, domain = ag[[3]]$sitetype)(sitetype)) %>%
    addCircleMarkers(data=ag[[4]], lng=~sa_lng, lat=~sa_lat, radius=~sqrt(sa_unassigned)*2, stroke = F,
                     opacity=1, fillOpacity=1,
                     group=as_vec[4],
                     popup=popuplabel_a[[4]], popupOptions = popupOptions(closeButton = T),
                     color=~colorFactor(palette = colfn2, domain = ag[[4]]$sitetype)(sitetype)) %>%
    addCircleMarkers(data=ag[[5]], lng=~sa_lng, lat=~sa_lat, radius=~sqrt(sa_unassigned)*2, stroke = F,
                     opacity=1, fillOpacity=1,
                     group=as_vec[5],
                     popup=popuplabel_a[[5]], popupOptions = popupOptions(closeButton = T),
                     color=~colorFactor(palette = colfn2, domain = ag[[5]]$sitetype)(sitetype)) %>%
    addCircleMarkers(data=ag[[6]], lng=~sa_lng, lat=~sa_lat, radius=~sqrt(sa_unassigned)*2, stroke = F,
                     opacity=1, fillOpacity=1,
                     group=as_vec[6],
                     popup=popuplabel_a[[6]], popupOptions = popupOptions(closeButton = T),
                     color=~ifelse(length(unique(ag[[6]]$sitetype))==2,
                                   colorFactor(palette = colfn2, domain = ag[[6]]$sitetype)(sitetype),
                                   colorFactor(palette = colfn1, domain = ag[[6]]$sitetype)(sitetype))) %>%
    addCircleMarkers(data=ag[[7]], lng=~sa_lng, lat=~sa_lat, radius=~sqrt(sa_unassigned)*2, stroke = F,
                     opacity=1, fillOpacity=1,
                     group=as_vec[7],
                     popup=popuplabel_a[[7]], popupOptions = popupOptions(closeButton = T),
                     color=~colorFactor(palette = colfn2, domain = ag[[7]]$sitetype)(sitetype)) %>%
    addCircleMarkers(data=ag[[8]], lng=~sa_lng, lat=~sa_lat, radius=~sqrt(sa_unassigned)*2, stroke = F,
                     opacity=1, fillOpacity=1,
                     group=as_vec[8],
                     popup=popuplabel_a[[8]], popupOptions = popupOptions(closeButton = T),
                     color=~colorFactor(palette = colfn2, domain = ag[[8]]$sitetype)(sitetype)) %>%
    addCircleMarkers(data=ag[[9]], lng=~sa_lng, lat=~sa_lat, radius=~sqrt(sa_unassigned)*2, stroke = F,
                     opacity=1, fillOpacity=1,
                     group=as_vec[9],
                     popup=popuplabel_a[[9]], popupOptions = popupOptions(closeButton = T),
                     color=~colorFactor(palette = colfn2, domain = ag[[9]]$sitetype)(sitetype)) %>%
   addCircleMarkers(data=ag[[10]], lng=~sa_lng, lat=~sa_lat, radius=~sqrt(sa_unassigned)*2, stroke = F,
                     opacity=1, fillOpacity=1,
                     group=as_vec[10],
                     popup=popuplabel_a[[10]], popupOptions = popupOptions(closeButton = T),
                     color=~colorFactor(palette = colfn2, domain = ag[[10]]$sitetype)(sitetype)) %>%
    addCircleMarkers(data=ag[[11]], lng=~sa_lng, lat=~sa_lat, radius=~sqrt(sa_unassigned)*2, stroke = F,
                     opacity=1, fillOpacity=1,
                     group=as_vec[11],
                     popup=popuplabel_a[[11]], popupOptions = popupOptions(closeButton = T),
                     color=~colorFactor(palette = colfn2, domain = ag[[11]]$sitetype)(sitetype)) %>%
    addCircleMarkers(data=ag[[12]], lng=~sa_lng, lat=~sa_lat, radius=~sqrt(sa_unassigned)*2, stroke = F,
                     opacity=1, fillOpacity=1,
                     group=as_vec[12],
                     popup=popuplabel_a[[12]], popupOptions = popupOptions(closeButton = T),
                     color=~colorFactor(palette = colfn2, domain = ag[[12]]$sitetype)(sitetype)) %>%
    addCircleMarkers(data=ag[[13]], lng=~sa_lng, lat=~sa_lat, radius=~sqrt(sa_unassigned)*2, stroke = F,
                     opacity=1, fillOpacity=1,
                     group=as_vec[13],
                     popup=popuplabel_a[[13]], popupOptions = popupOptions(closeButton = T),
                     color=~colorFactor(palette = colfn2, domain = ag[[13]]$sitetype)(sitetype)) %>%
    addCircleMarkers(data=ag[[14]], lng=~sa_lng, lat=~sa_lat, radius=~sqrt(sa_unassigned)*2, stroke = F,
                     opacity=1, fillOpacity=1,
                     group=as_vec[14],
                     popup=popuplabel_a[[14]], popupOptions = popupOptions(closeButton = T),
                     color=~colorFactor(palette = colfn2, domain = ag[[14]]$sitetype)(sitetype)) %>%
    addCircleMarkers(data=ag[[15]], lng=~sa_lng, lat=~sa_lat, radius=~sqrt(sa_unassigned)*2, stroke = F,
                     opacity=1, fillOpacity=1,
                     group=as_vec[15],
                     popup=popuplabel_a[[15]], popupOptions = popupOptions(closeButton = T),
                     color=~colorFactor(palette = colfn2, domain = ag[[15]]$sitetype)(sitetype)) %>%
    addCircleMarkers(data=ag[[16]], lng=~sa_lng, lat=~sa_lat, radius=~sqrt(sa_unassigned)*2, stroke = F,
                     opacity=1, fillOpacity=1,
                     group=as_vec[16],
                     popup=popuplabel_a[[16]], popupOptions = popupOptions(closeButton = T),
                     color=~colorFactor(palette = colfn2, domain = ag[[16]]$sitetype)(sitetype)) %>%
    addCircleMarkers(data=ag[[17]], lng=~sa_lng, lat=~sa_lat, radius=~sqrt(sa_unassigned)*2, stroke = F,
                     opacity=1, fillOpacity=1,
                     group=as_vec[17],
                     popup=popuplabel_a[[17]], popupOptions = popupOptions(closeButton = T),
                     color=~colorFactor(palette = colfn2, domain = ag[[17]]$sitetype)(sitetype)) %>%
    addCircleMarkers(data=ag[[18]], lng=~sa_lng, lat=~sa_lat, radius=~sqrt(sa_unassigned)*2, stroke = F,
                     opacity=1, fillOpacity=1,
                     group=as_vec[18],
                     popup=popuplabel_a[[18]], popupOptions = popupOptions(closeButton = T),
                     color=~colorFactor(palette = colfn2, domain = ag[[18]]$sitetype)(sitetype)) %>%
    addCircleMarkers(data=ag[[19]], lng=~sa_lng, lat=~sa_lat, radius=~sqrt(sa_unassigned)*2, stroke = F,
                     opacity=1, fillOpacity=1,
                     group=as_vec[19],
                     popup=popuplabel_a[[19]], popupOptions = popupOptions(closeButton = T),
                     color=~colorFactor(palette = colfn2, domain = ag[[19]]$sitetype)(sitetype)) %>%
    addCircleMarkers(data=ag[[20]], lng=~sa_lng, lat=~sa_lat, radius=~sqrt(sa_unassigned)*2, stroke = F,
                     opacity=1, fillOpacity=1,
                     group=as_vec[20],
                     popup=popuplabel_a[[20]], popupOptions = popupOptions(closeButton = T),
                     color=~colorFactor(palette = colfn2, domain = ag[[20]]$sitetype)(sitetype)) %>%
    addLegend(data=ag[[1]], opacity=1, pal = pal2, values = ~sitetype, group = "circles", position = "bottomright") %>%
    addScaleBar() %>%
    addControl(rr, position = "topleft") %>%
    addLayersControl(
      baseGroups = dput(as_vec),
      options = layersControlOptions(collapsed=FALSE)
    )

  psnu_map_abm_uap <- leaflet(data=shape_df) %>%
    addTiles() %>%
    addPolygons(data=region_shp,fillColor = "grey",
                color='black', weight=1.8, opacity=1, fillOpacity = 0.01) %>%
    addPolygons(fillColor = ~pal_abm(abm_link),
                color='white', weight=1, opacity=0.6, fillOpacity = 0.55, popup = psnu_popup) %>%
    addLegend(pal = pal_abm, values = ~abm_link,
              labFormat = labelFormat(suffix = '%', between = '% - '),
              opacity = 0.6, title = 'ABM Linkage', position = "bottomright") %>%
    addCircleMarkers(data=agentmapf, lng=~sa_lng, lat=~sa_lat, radius=~sqrt(sa_unassign_prop)*5, stroke = F,
                     opacity=1, fillOpacity=1,popup=popuplabel2, color=~pal2(sitetype),
                     popupOptions = popupOptions(closeButton = T)) %>% 
    addLegend(data=agentmapf, opacity=1, pal = pal2, values = ~sitetype, group = "circles", position = "bottomright") %>%  
    addScaleBar() %>%
    addControl(rr, position = "topleft")
  
  # save map
  map_path <- file.path(datapath,"/Graph")
  mapnm_ua <- paste("psnu_abm_pop_ua2019q1q2_", "test", ".html",sep="")
  saveWidget(psnu_map_abm_ua, file.path(map_path,mapnm_ua))

################################################################################################################
################################################################################################################
leaflet(df) %>% addTiles() %>%
  addCircleMarkers(
    radius = ~ sqrt(quantity),
    stroke = FALSE, fillOpacity = 0.5
  )

# Map for all agesex stratas
Finaldf_total_agent_allag <- bind_rows(Finaldf_total_unassignedlst_sav,Finaldf_total_assignedlst_sav) %>% 
  group_by(siteid, sitetype) %>%
  summarise(sa_assigned = sum(assigned), sa_unassigned = sum(not_assigned), 
            sa_lat=mean(lat), sa_lng=mean(lng), na.rm=T) %>%
  ungroup() %>%
  select(siteid, sa_lat, sa_lng, sa_assigned, sa_unassigned, sitetype) %>% 
  rowwise() %>% 
  mutate(sa_unassign_prop=sa_unassigned/sum(sa_assigned, sa_unassigned)) %>% 
  ungroup()  %>% 
  mutate_if(is.numeric, list(~replace(., is.na(.), 0 )))

getwd()
saveRDS(Finaldf_total_agent_allag, "Finaldf_total_agent_allag2019q1q2.rds")
Finaldf_total_agent_allag <- readRDS("Finaldf_total_agent_allag2019q1q2.rds")

# map for all agesex stratas
setwd("C:/Users/onl4/Documents/AgentBasedModel2nd90/FromImran_MohammedMujawar07082019/Files_Code/Graph/Map08032019")
getwd()

# Merge for sitename
load(file = "metadat.RData")
load("linkgx.Rdata") # use sitekey

skmeta_dat <- inner_join(sitekey,metadat) %>%
  select(siteid,sitename)
Finaldf_total_agent_allag_snm <- left_join(Finaldf_total_agent_allag,skmeta_dat)
saveRDS(Finaldf_total_agent_allag_snm, "Finaldf_total_agent_allag_snm.rds")
Finaldf_total_agent_allag_snm <- readRDS("Finaldf_total_agent_allag_snm.rds")

library(htmlwidgets)
rr <- HTML('<title> TITLE </title>')  

  agentmapf <- Finaldf_total_agent_allag_snm %>% arrange(siteid)
  
  colfn2 <- colorRampPalette(c('#cc5236', '#335b8e'))(2)
  pal2 <- colorFactor(palette = colfn2, domain = agentmapf$sitetype)
  
  state_popup <- paste0("<strong>SNU1: </strong>", region_shp$NAME)
  
  # table popup for unassigned map
  popuplabel3 <- paste0(
    '
    <b>', agentmapf$sitename,'</b><br><br>
    <b>Unassigned</b><br>
    Total : ', agentmapf$sa_unassigned,'<br>
    <b>Assigned</b><br>
    Total : ', agentmapf$sa_assigned,'<br><br>
    <b>Site ID : ', agentmapf$siteid,'<b><br><br>
    ')
  psnu_map_abm_ua <- leaflet(data=shape_df) %>%
    addTiles() %>%
    addPolygons(fillColor = ~pal_abm(abm_link), 
                color='white', weight=1, opacity=0.6, fillOpacity = 0.55) %>% 
    addPolygons(data=region_shp,fillColor = "grey", 
                color='black', weight=1.5, opacity=0.6, fillOpacity = 0.01, popup = state_popup) %>% 
    addLegend(pal = pal_abm, values = ~abm_link,
              labFormat = labelFormat(suffix = '%', between = '% - '),
              opacity = 0.6, title = 'ABM Linkage', position = "bottomright") %>%
    addCircleMarkers(data=agentmapf, lng=~sa_lng, lat=~sa_lat, radius=~sqrt(sa_unassigned)*0.5, stroke = F,
                     opacity=1, fillOpacity=1, color=~pal2(sitetype), popup=popuplabel3,
                     popupOptions = popupOptions(closeButton = T)) %>% 
    addLegend(data=agentmapf, opacity=1, pal = pal2, values = ~sitetype, group = "circles", position = "bottomright") %>%  
    addScaleBar() %>%
    addControl(rr, position = "topleft")
  
  # table popup for unassigned proportion map
  popuplabel4 <- paste0(
    '
    <b>', agentmapf$sitename,'</b><br><br>
    <b>Proportion of Unassigned</b><br>
    Total : ', round(agentmapf$sa_unassign_prop*100,1),'%<br>
    <b>Unassigned</b><br>
    Total : ', agentmapf$sa_unassigned,'<br>
    <b>Assigned</b><br>
    Total : ', agentmapf$sa_assigned,'<br><br>
    <b>Site ID : ', agentmapf$siteid,'<b><br><br>
    ')
  psnu_map_abm_uap <- leaflet(data=shape_df) %>%
    addTiles() %>%
    addPolygons(fillColor = ~pal_abm(abm_link), 
                color='white', weight=1, opacity=0.6, fillOpacity = 0.55) %>% 
    addPolygons(data=region_shp,fillColor = "grey", 
                color='black', weight=1.5, opacity=0.6, fillOpacity = 0.01, popup = state_popup) %>% 
    addLegend(pal = pal_abm, values = ~abm_link,
              labFormat = labelFormat(suffix = '%', between = '% - '),
              opacity = 0.6, title = 'ABM Linkage', position = "bottomright") %>%
    addCircleMarkers(data=agentmapf, lng=~sa_lng, lat=~sa_lat, radius=~sqrt(sa_unassign_prop)*5, stroke = F,
                     opacity=1, fillOpacity=1, color=~pal2(sitetype), popup=popuplabel4,
                     popupOptions = popupOptions(closeButton = T)) %>% 
    addLegend(data=agentmapf, opacity=1, pal = pal2, values = ~sitetype, group = "circles", position = "bottomright") %>%  
    addScaleBar() %>%
    addControl(rr, position = "topleft")
  
  # save map
  mapnm_ua <- paste("psnu_abm_pop_ua_allAgesex", ".html", sep="")
  mapnm_uap <- paste("psnu_abm_pop_uaprop_allAgesex", ".html", sep="")
  saveWidget(psnu_map_abm_ua, mapnm_ua)
  saveWidget(psnu_map_abm_uap, mapnm_uap)
#############################################################################
# heatmap
  load("finallist_facil_dis.RData")
  Finaldf <- dplyr::bind_rows(finallist_facil_dis)

  ## SNU1 level ##
  snu_all_link_ag <- Finaldf %>% 
    select(snu1_tst, snu1uid_tst, agesex,
           tot_rx, tx_new,	hts_pos) %>% 
    group_by(snu1_tst, snu1uid_tst, agesex) %>% 
    summarize_all(list(~sum), na.rm=T) %>% 
    ungroup() %>% 
    mutate(prox_link = round((tx_new/hts_pos)*100,0),
           abm_link = round((tot_rx/hts_pos)*100,0)) %>%
  # Remove NA and NaNs
    mutate_if(is.double, list(~replace(., is.na(.), NA_real_))) %>%
    mutate_if(is.double, list(~replace(., is.nan(.), NA_real_))) %>%
    mutate_if(is.double, list(~replace(., .>999999999, NA_real_)))
  
    snu_all_link_ag.heatmap <- ggplot(snu_all_link_ag, mapping = aes(x=agesex, y=snu1_tst)) +
    geom_tile((aes(fill = abm_link)) ) +
    geom_text(aes(label = abm_link), size = 4, colour = "black") +  # "turquoise4", "cyan4"
    scale_fill_gradientn(name = "abm_link",colours = rainbow(6)) + # viridis(256, option = "D")
    coord_fixed()   + 
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5),axis.title.x = element_blank(),
          axis.title.y = element_blank(), axis.text.x=element_text(angle=90,vjust = 0.5))  +  # Remove y-axis title)
    ggtitle(label = "ABM by SNU1 Heatmap") + 
    scale_x_discrete(position = "top", labels = abbreviate )
    
    ggsave("ABM_SNU1_Heatmap_rainbow6_classic_dpi600_08052019.png", snu_all_link_ag.heatmap, scale=3, path=snu_heatmap_path, dpi = 600)
    
## SNU2 - PSNU level ##
    psnu_all_link_ag <- Finaldf %>% 
      select(psnu_tst, psnuuid_tst, agesex,
             tot_rx, tx_new,	hts_pos) %>% 
      group_by(psnu_tst, psnuuid_tst, agesex) %>% 
      summarize_all(list(~sum), na.rm=T) %>% 
      ungroup() %>% 
      mutate(prox_link = round((tx_new/hts_pos)*100,0),
             abm_link = round((tot_rx/hts_pos)*100,0)) %>% 
      # Remove NA and NaNs
      mutate_if(is.double, list(~replace(., is.na(.), NA_real_))) %>% 
      mutate_if(is.double, list(~replace(., is.nan(.), NA_real_))) %>% 
      mutate_if(is.double, list(~replace(., .>999999999, NA_real_)))
    
    colnames(psnu_all_link_ag)
    str(psnu_all_link_ag)
    psnuvec <- unique(psnu_all_link_ag$psnu_tst)
    length(unique(psnu_all_link_ag$psnu_tst))

    psnu_all_link_ag.heatmap1 <- psnu_all_link_ag %>%
      filter(psnu_tst %in% psnuvec[1:32]) %>%
      ggplot(mapping = aes(x=agesex, y=psnu_tst)) +
      geom_tile((aes(fill = abm_link)) ) +
      geom_text(aes(label = abm_link), size = 4, colour = "cyan4") +  # "turquoise4",  "cyan4"
      scale_fill_gradientn(name = "abm_link",colours = plasma(256)) + # viridis(256, option = "D")
      coord_fixed()   +
      # theme_classic() +
      theme(plot.title = element_text(hjust = 0.5),axis.title.x = element_blank(),
            axis.title.y = element_blank(), axis.text.x=element_text(angle=90,vjust = 0.5))  +  # Remove y-axis title)
      ggtitle(label = "ABM by PSNU -SNU2 Heatmap") + 
      scale_x_discrete(position = "top", labels = abbreviate ) 


# for all PSNUs - 16 heatmaps
    path <- "C:\\Users\\onl4\\Documents\\AgentBasedModel2nd90\\FromImran_MohammedMujawar07082019\\Files_Code"
    heatmap_path <- file.path(getwd(),"Graph/Heatmap")
    setwd(heatmap_path)
    snu_heatmap_path <- file.path(heatmap_path, "SNU1")
    psnu_heatmap_path <- file.path(heatmap_path, "PSNU")
    psnu_heatmap_path2 <- file.path(heatmap_path, "PSNU_rainbow")
    setwd(psnu_heatmap_path)
    getwd()
    
    subpsnu_idx <- split(1:506, rep(1:16, c(rep(32,15),26)))
    for (i in seq(1,length(subpsnu_idx))){
      pngnm <- paste("psnu_all_link_ag_heatmap_",length(subpsnu_idx[[i]]),"_psnus", "_", i, ".png", sep = '')
      heatmap <-  psnu_all_link_ag %>%
        filter(psnu_tst %in%  psnuvec[c(subpsnu_idx[[i]])]) %>%
        ggplot(mapping = aes(x=agesex, y=psnu_tst)) +
        geom_tile((aes(fill = abm_link)) ) +
        geom_text(aes(label = abm_link), size = 4, colour = "black") +  #  "cyan4","turquoise4"
        # scale_fill_gradientn(name = "abm_link",colours = plasma(256)) + # viridis(256, option = "D")
        scale_fill_gradientn(name = "abm_link",colours = rainbow(6)) + # viridis(256, option = "D")
        coord_fixed()   +
        # theme_classic() +
        theme(plot.title = element_text(hjust = 0.5),axis.title.x = element_blank(),
              axis.title.y = element_blank(), axis.text.x=element_text(angle=90,vjust = 0.5))  +  # Remove y-axis title)
        ggtitle(label = "ABM by PSNU -SNU2 Heatmap") + 
        scale_x_discrete(position = "top", labels = abbreviate) 
        # ggsave(pngnm, heatmap, scale=2, path=psnu_heatmap_path)
        ggsave(pngnm, heatmap, scale=3, path=psnu_heatmap_path2, dpi = 600)
    }
    
    # for all PSNUs - By SNU1 - 35 heatmaps
    path <- "C:\\Users\\onl4\\Documents\\AgentBasedModel2nd90\\FromImran_MohammedMujawar07082019\\Files_Code"
    heatmap_path <- file.path(getwd(),"Graph/Heatmap")
    setwd(heatmap_path)
    snu_heatmap_path <- file.path(heatmap_path, "SNU1")
    psnu_heatmap_path <- file.path(heatmap_path, "PSNU")
    # psnu_heatmap_path2 <- file.path(heatmap_path, "PSNU_rainbow")
    psnu_heatmap_path3 <- file.path(heatmap_path, "PSNUBySNU1_rainbow")
    setwd(heatmap_path)
    getwd()
    
    snu1vec <- unique(Finaldf$snu1_tst)
    length(snu1vec) # 35 SNU1
    psnuvec <- unique(psnuBySNU1_all_link_ag$psnu_tst)
    length(psnuvec) 
    
    ## SNU2 - By SNU1 - PSNU level  ##
    psnuBySNU1_all_link_ag <- Finaldf %>% 
      select(snu1_tst, snu1uid_tst, psnu_tst, psnuuid_tst, agesex,
             tot_rx, tx_new,	hts_pos) %>% 
      group_by(snu1_tst, snu1uid_tst, psnu_tst, psnuuid_tst, agesex) %>% 
      summarize_all(list(~sum), na.rm=T) %>% 
      ungroup() %>% 
      mutate(prox_link = round((tx_new/hts_pos)*100,0),
             abm_link = round((tot_rx/hts_pos)*100,0)) %>% 
      # Remove NA and NaNs
      mutate_if(is.double, list(~replace(., is.na(.), NA_real_))) %>% 
      mutate_if(is.double, list(~replace(., is.nan(.), NA_real_))) %>% 
      mutate_if(is.double, list(~replace(., .>999999999, NA_real_)))
    
    for (i in seq(1,length(snu1vec))){
        heatmap <-  psnuBySNU1_all_link_ag %>%
        filter(snu1_tst %in%  snu1vec[i]) %>%
        ggplot(mapping = aes(x=agesex, y=psnu_tst)) +
        geom_tile((aes(fill = abm_link)) ) +
        geom_text(aes(label = abm_link), size = 4, colour = "black") +  #  "cyan4","turquoise4"
        scale_fill_gradientn(name = "abm_link",colours = rainbow(6)) + # viridis(256, option = "D")
        coord_fixed()   +
        theme(plot.title = element_text(hjust = 0.5),axis.title.x = element_blank(),
              axis.title.y = element_blank(), axis.text.x=element_text(angle=90,vjust = 0.5))  +  # Remove y-axis title)
        ggtitle(label = paste("ABM by PSNU", "in", "SNU1 -", snu1vec[i], "Heatmap",sep=" ")) + 
        scale_x_discrete(position = "top", labels = abbreviate)
        pngnm <- paste("psnuBySNU1_all_link_ag_heatmap", i, "_Total_", length(unique(heatmap$data$psnu_tst)),"PSNUs_", "In_SNU1_", snu1vec[i], ".png", sep = '')
        ggsave(pngnm, heatmap, scale=3, path=psnu_heatmap_path3, dpi = 600)
    }
    

############################################################################
# Spider net map
# choose data folder
datapath <- "C:\\Users\\onl4\\Documents\\AgentBasedModel2nd90\\ouput08132019"
setwd(datapath)
getwd()
# prepare data
load("finallist_facil_dis.RData")
load("total_unassignedlst_sav.RData")
load("total_assignedlst_sav.RData")
    
Finaldf <- dplyr::bind_rows(finallist_facil_dis)
Finaldf_total_unassignedlst_sav <- dplyr::bind_rows(total_unassignedlst_sav)
Finaldf_total_assignedlst_sav <- dplyr::bind_rows(total_assignedlst_sav)

Finaldf_total_as_uas <- bind_rows(Finaldf_total_unassignedlst_sav,Finaldf_total_assignedlst_sav)

finaldf_assign_agent <- Finaldf_total_as_uas %>% 
  select(siteid,lat,lng) %>% 
  unique() %>% 
  group_by(siteid) %>%
  summarise(latx=mean(lat,  na.rm = TRUE), lngx=mean(lng, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(siteid)

finaldf_assign_ext_int <- Finaldf %>%
  filter(!(is.na(ext_rx) & is.na(int_rx))) %>%
  select(-c(untreated, tx_new, hts_pos, lng_tst, lat_tst, lng_rx, lat_rx)) %>%
  left_join(finaldf_assign_agent, by=c("mod_tst_id"="siteid"))

finaldf_assign_ext_int_cord_cksites <- finaldf_assign_ext_int  %>%
  left_join(finaldf_assign_agent,by=c("mod_rx_id"="siteid")) 

# Go back to original working directory
setwd(workpath)
getwd()

# Adding on PSNU mean lat long; fortity flatens the spatial data
#psnu polygon has a number lat long associated with it.create a unique id to maintain the mean for a psnu
# Use row numbers to create an id so that each row is unique
psnu_shpx <- readRDS("Nigeria_State_LGA.rds")
psnu_shp <- psnu_shpx[!is.na(psnu_shpx$uid),]

psnu_shp@data$id <- rownames(psnu_shp@data)
coord2 <- fortify(psnu_shp, region = 'id')
coord2x <- coord2 %>% group_by(id) %>% summarise(lat=mean(lat), lng=mean(long)) %>% ungroup()

shape_lat_lng <- merge(psnu_shp@data,coord2x, by='id')

psnu_lat_long <- shape_lat_lng %>% 
  select(uid, lat, lng) %>% 
  rename(psnuuid = uid) %>% 
  mutate(psnuuid = as.character(psnuuid))
colnames(psnu_lat_long)

# Community sites and sites without lat long are give PSNU centroid lat long
finaldf_assign_ext_int_latlng <- left_join(finaldf_assign_ext_int, psnu_lat_long, by=c("psnuuid_tst"="psnuuid"))
summary(finaldf_assign_ext_int_latlng)

# Giving sites without coordinates, PSNU centroid lat long
colnames(finaldf_assign_ext_int_latlng)
finaldf_assign_ext_int_cordinate <- finaldf_assign_ext_int_latlng %>% 
  mutate(lngy = ifelse(is.na(lngx), jitter(lng, factor = 0.001), lngx),
         laty = ifelse(is.na(latx), jitter(lat, factor = 0.001), latx)) %>% 
  select(-c(lngx, latx, lng, lat)) %>%
  mutate(lat_tst = round(laty, 6),
         lng_tst = round(lngy, 6)) %>% 
  select(-c(laty, lngy)) 

# table(finaldf_assign_ext_int_cordinate$PSNU_lng)
summary(finaldf_assign_ext_int_cordinate)
colnames(finaldf_assign_ext_int_cordinate)

finaldf_assign_ext_int_cord_2sites <- finaldf_assign_ext_int_cordinate  %>%
  left_join(finaldf_assign_agent,by=c("mod_rx_id"="siteid"))  %>%
  left_join(psnu_lat_long, by=c("psnuuid_rx"="psnuuid")) %>% 
  mutate(lngy = ifelse(is.na(lngx), jitter(lng, factor = 0.001), lngx),
         laty = ifelse(is.na(latx), jitter(lat, factor = 0.001), latx)) %>% 
  select(-c(lngx, latx, lng, lat)) %>%
  mutate(lat_rx = round(laty, 6),
         lng_rx = round(lngy, 6)) %>% 
  select(-c(laty, lngy)) 

colnames(finaldf_assign_ext_int_cord_2sites)
summary(finaldf_assign_ext_int_cord_2sites)

# save finaldf_assign_ext_int_cord_2sites for 2019 Q1 Q2
setwd(datapath); getwd()
saveRDS(finaldf_assign_ext_int_cord_2sites, "finaldf_assign_ext_int_cord_2sites_2019q1q2.rds")
finaldf_assign_ext_int_cord_2sites <- readRDS("finaldf_assign_ext_int_cord_2sites_2019q1q2.rds")

# remove those that tst and rx are same site
assign_ext <- finaldf_assign_ext_int_cord_2sites %>% 
  filter(ext_rx != 0) %>% 
  select(-c(ext_rx, int_rx)) %>% 
  mutate(clust_name1 = paste(uid_tst, "_", uid_rx, sep=''), 
         clust_name2 = paste(uid_rx, "_", uid_tst, sep=''))


assign_int <- finaldf_assign_ext_int_cord_2sites %>% 
  filter(int_rx != 0) %>% 
  select(-c(ext_rx, int_rx))

# by age-sex strata
# 20 age sex strata for 2019 Q1 Q2
agesexvec <- unique(assign_ext$agesex)
assign_ext_ag1 <-assign_ext %>% 
  filter(agesex %in% agesexvec[1]) %>% 
  select(mod_tst_id,mod_rx_id,uid_tst,uid_rx,tot_rx,sitename_tst,sitetype_tst,sitename_rx,sitetype_rx,
         lat_tst,lng_tst,lat_rx,lng_rx,clust_name1, clust_name2) %>%
  rename(from = uid_tst, to = uid_rx)

length(unique(assign_ext_ag1$from)) 
length(unique(assign_ext_ag1$to))  
length(unique(assign_ext_ag1$clust_name1)) 
colnames(assign_ext_ag1)

# ****************************************************************** #
#################### Nodes ###########################################
# ****************************************************************** #

alter <- assign_ext_ag1 %>% 
  select(from, sitename_tst, sitetype_tst, lat_tst, lng_tst, tot_rx) %>% 
  group_by(from, sitename_tst, sitetype_tst) %>%
  summarise(lat=mean(lat_tst,na.rm=TRUE),lng=mean(lng_tst,na.rm=TRUE), tot=sum(tot_rx,na.rm=TRUE)) %>%
  ungroup() %>%
  unique() %>% 
  mutate(vertex_type = "alter", tot_in=0) %>% 
  rename(id = from,sitename=sitename_tst,sitetype=sitetype_tst, tot_out=tot)

hubs <- assign_ext_ag1 %>% 
  select(to, sitename_rx, sitetype_rx, lat_rx, lng_rx, tot_rx) %>% 
  group_by(to, sitename_rx, sitetype_rx) %>%
  summarise(lat=mean(lat_rx,na.rm=TRUE),lng=mean(lng_rx,na.rm=TRUE), tot=sum(tot_rx,na.rm=TRUE)) %>%
  ungroup() %>% 
  unique() %>% 
  mutate(vertex_type = "hub", tot_out=0) %>% 
  rename(id = to,sitename=sitename_rx,sitetype=sitetype_rx, tot_in=tot)

clus_info <- bind_rows(alter, hubs) # 1535 # 837 - 2019

alter_hubs <- alter %>% 
  inner_join(hubs, by="id") %>% 
  select(-c(tot_in.x, tot_out.y, vertex_type.x, vertex_type.y, sitename.y,sitetype.y,lat.y,lng.y)) %>% 
  mutate(vertex_type="alter_hub") %>% 
  rename(tot_out=tot_out.x, tot_in=tot_in.y, sitename=sitename.x, sitetype=sitetype.x, lat=lat.x, lng=lng.x)

clus_info_no2mv <- clus_info %>% 
  filter(id %ni% alter_hubs$id)

mv_site <- c(clus_info_no2mv$id,alter_hubs$id) 

# node alone
assign_int_ag1 <-assign_int %>% 
  filter(agesex %in% agesexvec[1]) %>% 
  select(mod_tst_id,mod_rx_id,uid_tst,uid_rx,tot_rx,sitename_tst,sitetype_tst,sitename_rx,sitetype_rx,
         lat_tst,lng_tst,lat_rx,lng_rx) %>%
  rename(from = uid_tst, to = uid_rx)

length(unique(assign_int_ag1$from)) 
length(unique(assign_int_ag1$to))   
colnames(assign_int_ag1)

alter_int <- assign_int_ag1 %>% 
  select(from, sitename_tst, sitetype_tst, lat_tst, lng_tst, tot_rx) %>% 
  group_by(from, sitename_tst, sitetype_tst) %>%
  summarise(lat=mean(lat_tst,na.rm=TRUE),lng=mean(lng_tst,na.rm=TRUE), tot=sum(tot_rx,na.rm=TRUE)) %>%
  ungroup() %>%
  unique() %>% 
  mutate(vertex_type = "alter_int") %>% 
  rename(id = from,sitename=sitename_tst,sitetype=sitetype_tst, tot_internal=tot) 

colnames(alter_int)
colnames(alter_hubs)

alter_alone <- alter_int %>% 
  filter(id %ni% mv_site) %>% 
  mutate(vertex_type = "alter_alone") %>% 
  mutate(tot_out=0, tot_in=0)

nodes_1n2_mv <- bind_rows(clus_info_no2mv,alter_hubs)
nodes_1n2_mv_int <- alter_int  %>% 
  select(id,tot_internal)  %>%
  right_join(nodes_1n2_mv,by="id") %>% 
  mutate_if(is.numeric, list(~replace(., is.na(.), 0 )))

table(nodes_1n2_mv_int$tot_internal, useNA="ifany")
  

total_site <- c(nodes_1n2_mv_int$id,alter_alone$id)
nodes_all <- bind_rows(nodes_1n2_mv_int,alter_alone) %>% 
  rowwise() %>% 
  mutate(tot=sum(tot_internal,tot_in,tot_out,na.rm=TRUE))

nodes_all_combTyp <- nodes_all %>% 
  mutate(vertex_site_type=paste(sitetype, toupper(vertex_type), sep=': '))

saveRDS(nodes_all_combTyp, "nodes_all_combTyp2019q1q2_08142019.rds")
nodes_all_combTyp <- readRDS("nodes_all_combTyp2019q1q2_08142019.rds")

# *******************************************************************#
#################### Edges ###########################################
# *******************************************************************#
# modify the coordinates for two way lines
# colnames(assign_ext_ag1)
assign_ext_ag1_nomodify <- assign_ext_ag1 %>% 
  mutate(rwid=row_number(), grp=1) %>% 
  filter(clust_name1 %ni% assign_bothway_uniq2$clust_name1)

assign_bothway <- assign_ext_ag1 %>% 
  mutate(rwid=row_number()) %>% 
  filter(clust_name1 %in% clust_name2)

if (dim(assign_bothway)[1] != 0) {
  assign_bothway_uniq1 <- assign_bothway %>%
    mutate(dx = pmin(from, to), dy = pmax(from, to)) %>%
    distinct(dx, dy) %>%
    mutate(clust_name1 = paste(dx, "_", dy, sep='')) %>% 
    select(-dx,-dy)

  assign_bothway_uniq2 <-  assign_bothway %>% 
    select(clust_name1) %>% 
    filter(clust_name1 %ni% assign_bothway_uniq1$clust_name1)
  
  assign_ext_ag1_modify <- assign_ext_ag1 %>% 
    mutate(rwid=row_number(), grp=2) %>% 
    filter(clust_name1 %in% assign_bothway_uniq2$clust_name1) %>% 
    mutate(lat_tst_mod=lat_tst+0.000001,
           lng_tst_mod=lng_tst+0.000001,
           lat_rx_mod=lat_rx+0.000001,
           lng_rx_mod=lng_rx+0.000001) %>% 
    select(-c(lat_tst,lng_tst,lat_rx,lng_rx)) %>% 
    rename(lat_tst=lat_tst_mod,lng_tst=lng_tst_mod,lat_rx=lat_rx_mod,lng_rx=lng_rx_mod)

  assign_ext_ag1_final <- bind_rows(assign_ext_ag1_nomodify, assign_ext_ag1_modify) %>% 
    arrange(grp,rwid) %>% 
    select(-clust_name2) %>% 
    rename(clust_name=clust_name1)
} else {
    assign_ext_ag1_final <- assign_ext_ag1_nomodify %>%
    arrange(rwid) %>% 
    select(-clust_name2) %>% 
    rename(clust_name=clust_name1)
  }


# 2019
saveRDS(assign_ext_ag1_final, "assign_ext_ag1_final2019q1q2_08142019.rds")
assign_ext_ag1_final <- readRDS("assign_ext_ag1_final2019q1q2_08142019.rds")

# Creating dataset for creating network lines
edge_lines1 <- assign_ext_ag1_final %>% 
  select(rwid, grp, lng_tst, lat_tst, clust_name, tot_rx, sitename_tst, sitename_rx) %>% 
  unique() %>% 
  rename(lngs = lng_tst,
         lats = lat_tst)

edge_lines2 <- assign_ext_ag1_final %>% 
  select(rwid, grp, lng_rx, lat_rx, clust_name, tot_rx, sitename_tst, sitename_rx) %>% 
  unique() %>% 
  rename(lngs = lng_rx,
         lats = lat_rx)

edge_lines <- bind_rows(edge_lines1, edge_lines2) 

# creating a dummy dataset to break the line flow
dummy_na <- as.data.frame(cbind(rwid=c(1:dim(assign_ext_ag1_final)[1]), lngs=NA, lats=NA))
edge_lines_na <- bind_rows(edge_lines, dummy_na) %>% 
  arrange(rwid, lngs, lats)

# 2019
saveRDS(edge_lines_na, "edge_lines_na2109q1q2_08142019.rds")
edge_lines_na <- readRDS("edge_lines_na2109q1q2_08142019.rds")

# if group number is 2, two way lines exist
num_grp <- length(unique(assign_ext_ag1_final$grp))
if (num_grp==2) {
  edge_lines_na_grp1 <- edge_lines_na %>% 
    filter(grp==1)
  edge_lines_na_grp2 <- edge_lines_na %>% 
    filter(grp==2)
}

## Spider Map ##

# Calculate adjusted and proxy linkage values at PSNU level
psnulink <- Finaldf %>% 
  select(psnu_tst, psnuuid_tst,
         tot_rx, tx_new,	hts_pos) %>% 
  group_by(psnu_tst, psnuuid_tst) %>% 
  summarize_all(list(~sum), na.rm=T) %>% 
  ungroup() %>% 
  mutate(prox_link = round((tx_new/hts_pos)*100,0),
         abm_link = round((tot_rx/hts_pos)*100,0)) %>% 
  # Remove NA and NaNs
  mutate_if(is.double, list(~replace(., is.na(.), NA_real_))) %>% 
  mutate_if(is.double, list(~replace(., is.nan(.), NA_real_))) %>% 
  mutate_if(is.double, list(~replace(., .>999999999, NA_real_)))

# pulling in the PSNU shape files (in R dataset) and
# Cleaning up PSNU shape files, taking out NA values
# set working directory to default
setwd(workpath)
psnu_shpx <- readRDS("Nigeria_State_LGA.rds")
psnu_shp <- psnu_shpx[!is.na(psnu_shpx$uid),]

# Read and clean up Region shape files, taking out NA values
region_shpx <- readRDS("Nigeria_State_shp.rds")
region_shp <- region_shpx[!is.na(region_shpx$ID),]

shape_df <- merge(psnu_shp, psnulink, by.x = "uid", by.y = "psnuuid_tst")

n_psnu_abm_link <- length(unique(shape_df$abm_link))
c_abm <- colorRampPalette(c('#cc5234', '#b5b867', '#6ca18f'))(n_psnu_abm_link)
pal_abm <- colorNumeric(palette = c_abm, domain = shape_df$abm_link)

setwd(datapath)
library(htmlwidgets)
rr <- HTML('<title> TITLE </title>')

num_type <- length(unique(nodes_all_combTyp$vertex_site_type))
colfn2 <- colorRampPalette(c("#6CF18F", "#A379FB", "#946A79", "#CC5234", "#335B8E"))(num_type)
pal2 <- colorFactor(palette = colfn2, domain = nodes_all_combTyp$vertex_site_type)
popuplabel1 <- paste0(
  '
  <b>',nodes_all_combTyp$sitename,'</b><br><br>
  <b>Assigned</b><br>
  Internal : ', nodes_all_combTyp$tot_internal, '<br>
  External : ', nodes_all_combTyp$tot_out, '<br>
  Transfer-In : ', nodes_all_combTyp$tot_in, '<br>
  Total : ', nodes_all_combTyp$tot,'<br><br>
  <b>Net Typer : ',  nodes_all_combTyp$vertex_site_type,'<b><br><br>
  ')
if (num_grp==1) {
  map_spider <- leaflet(data=shape_df) %>%
    addTiles() %>%
    addPolygons(fillColor = ~pal_abm(abm_link), 
                color='white', weight=1, opacity=0.6, fillOpacity = 0.53) %>%  # , popup = psnu_popup
    addPolygons(data=region_shp,fillColor = "grey", 
                color='black', weight=1.5, opacity=0.4, fillOpacity = 0.01) %>% 
    addLegend(pal = pal_abm, values = ~abm_link,
              labFormat = labelFormat(suffix = '%', between = '% - '),
              opacity = 0.6, title = 'ABM Linkage', position = "bottomright") %>%
    addCircleMarkers(data=nodes_all_combTyp, lng=~lng, lat=~lat, radius=0.5, opacity=1,
                     color=~pal2(vertex_site_type), fillOpacity=1, 
                     popup=popuplabel1,
                     popupOptions = popupOptions(closeButton = T)) %>%
    addLegend(data=nodes_all_combTyp, pal = pal2, values = ~vertex_site_type, title = 'Site & Net Type',
              group = "circles", position = "bottomright") %>% 
    addPolylines(data=edge_lines_na, lng = ~lngs, lat = ~lats, group = ~rwid,
                 weight = 0.5, fillOpacity = 0.5, color="blue") %>%  # "purple"
    addScaleBar() %>%
    addControl(rr, position = "topleft")
} else {
  map_spider <- leaflet(data=shape_df) %>%
    addTiles() %>%
    addPolygons(fillColor = ~pal_abm(abm_link), 
                color='white', weight=1, opacity=0.6, fillOpacity = 0.53) %>%  # , popup = psnu_popup
    addPolygons(data=region_shp,fillColor = "grey", 
                color='black', weight=1.5, opacity=0.4, fillOpacity = 0.01) %>% 
    addLegend(pal = pal_abm, values = ~abm_link,
              labFormat = labelFormat(suffix = '%', between = '% - '),
              opacity = 0.6, title = 'ABM Linkage', position = "bottomright") %>%
    addCircleMarkers(data=nodes_all_combTyp, lng=~lng, lat=~lat, radius=0.5, opacity=1,
                     color=~pal2(vertex_site_type), fillOpacity=1, 
                     popup=popuplabel1,
                     popupOptions = popupOptions(closeButton = T)) %>%
    addLegend(data=nodes_all_combTyp, pal = pal2, values = ~vertex_site_type,  title = 'Site & Net Type', 
              group = "circles", position = "bottomright") %>% 
    addPolylines(data=edge_lines_na_grp1, lng = ~lngs, lat = ~lats, group = ~rwid,
                 weight = 0.5, fillOpacity = 0.5, color="blue") %>%
    addPolylines(data=edge_lines_na_grp2, lng = ~lngs, lat = ~lats, group = ~rwid,
                 weight = 0.5, fillOpacity = 0.5, color="red") %>%
    addScaleBar() %>%
    addControl(rr, position = "topleft")
}
