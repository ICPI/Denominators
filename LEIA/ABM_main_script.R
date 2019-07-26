
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

# Load functions required for model
comsplay <- dget("comsplay.R")
qchoice <- dget("qchoice.R")
abm_agesex <- dget("abm_agesex.R")
abm_q <- dget("abm_q.R")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Load (install, if required!) packages ~~~~~~~===============
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#  Load packages, and install them if they are not already installed, before loading
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
         "tools"
) )


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Dummy data required for Choice matrix function ====
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  rank vectors used in the choice matrix function
rankvec <- paste("c", formatC(seq(1:100), width=3, flag=0), sep="")

dummyrank <- as.data.frame(rankvec) %>% 
  mutate(value=as.integer(0)) %>% 
  spread(rankvec, value)

dummyfl <- dummyrank[FALSE, ]



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

names(foonames)
# Creating the vector of column types
colvecx <- as.vector(ifelse(grepl("qtr|cumulative|targets", foonames), "d", "c"))
colvec <- paste(colvecx, collapse = '')


# Pulling in the data with correct datatype for variables  
msd <- read_tsv(file=glist, 
                  col_names = TRUE,
                  col_types = colvec)      # ending if-else for Genie check

names(msd) <- tolower(names(msd))  


# Getting site lat long dat
latlng <- readRDS("Lat_Long_Nigeria_FacilityReport_2018.rds") %>% 
  select(uid, Longitude, Latitude) %>% 
  mutate(uid = as.character(uid))


# Creating the site key
sitekey <- msd %>% 
  select(orgunituid) %>% 
  unique() %>% 
  mutate(siteid = row_number()) %>% 
  rename(uid = orgunituid)

# Creating data frame with metadata 
metadata <- msd %>% 
  select(orgunituid, sitename, sitetype, psnu, psnuuid, snu1, snu1uid) %>% 
  rename(uid = orgunituid) %>% 
  unique() 
  

# Creating data frame for facility lat long data with meta data
geodata <- left_join(metadata, latlng)


# pulling in the PSNU shape files (in R dataset)
psnu_shpx <- readRDS("Nigeria_State_LGA.rds")
# Cleaning up PSNU shape files, taking out NA values
psnu_shp <- psnu_shpx[!is.na(psnu_shpx$code),]

#region_shpx <- readRDS("Nigeria_State_shp.rds")
# Cleaning up Region shape files, taking out NA values
#region_shp <- region_shpx[!is.na(region_shpx$NAME),]

# check CRS projection for both 
# proj4string(psnu_shp)
# proj4string(region_shp)

# change CRS to 
# spTransform("<insert shapefile here>","+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")  

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Data Restructuring  ~~~~~~~===============
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Restructuring the dataset to get the Tx dataset and then the choice matrix 
linkgx <- msd %>% 
  filter(indicator %in% c("HTS_TST_POS","TX_NEW","TX_CURR")) %>% 
  gather(period, value, qtr1:qtr4) %>% 
  filter(!is.na(value)) %>% 
  mutate(agesex = paste(trendsfine, sex, sep="|")) %>% 
  # filter(trendsfine %in% agevec)%>%
  filter(fiscal_year %in% c("2019", "2018")) %>% 
  # filter(standardizedDisaggregate%in%c("")) %>% 
  filter(snu1!="_Military Nigeria") %>% 
  filter(snu1!=" ") %>% 
  # Cleaning up age and sex
  filter(!is.na(sex)) %>% 
  filter(!is.na(trendsfine)) %>% 
  filter(trendsfine %ni% c("Retired Age Band",
                           "Coarse",
                           "<05")) %>% 
  rename(uid=orgunituid)


# Checking the age-sex vector for loop
unique(linkgx$agesex)

# Creating age-sex vector to loop through the ABM for each age-sex category
agesexvec <- unique(linkgx$agesex)

  
linkg <- linkgx %>%   
  # filter(agesex %in% k) %>% 
  mutate(period = gsub("tr", "", period)) %>% 
  mutate(timept = paste(fiscal_year, period, sep="_")) %>%
  select(agesex, psnu, psnuuid, uid, sitename, sitetype, indicator, timept, value) %>% 
  group_by_if(is.character) %>% 
  summarise(value = sum(value, na.rm=T)) %>% 
  ungroup() %>% 
  filter(!is.na(value)) %>% 
  filter(value!=0) %>% 
  group_by_if(is.character) %>% 
  summarise(value = sum(value, na.rm=T)) %>% 
  spread(indicator, value) %>% 
  ungroup()


# Merging the lat long data onto MER dataset
df <- left_join(linkg, latlng)


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
df1 <- left_join(df, psnu_lat_long, by="psnuuid") %>% 
  select(-psnu.y) %>% rename(psnu = psnu.x)

# Giving sites without coordinates, PSNU centroid lat long
df2x <- df1 %>% 
  mutate(lngx = ifelse(is.na(Longitude), jitter(df1$lng, factor = 0.001), Longitude),
         latx = ifelse(is.na(Latitude), jitter(df1$lat, factor = 0.001), Latitude)) %>% 
  mutate(PSNU_lng = ifelse(is.na(Longitude), 1, 0),
         PSNU_lat = ifelse(is.na(Latitude), 1, 0)) %>%
  select(-c(Longitude, Latitude, lng, lat)) %>% 
  mutate(lat = round(latx, 6),
         lng = round(lngx, 6)) %>% 
  select(-c(latx, lngx)) 

df_dummy <- df1[FALSE,FALSE] %>% 
  mutate(TX_NEW = NA,
         TX_CURR = NA,
         HTS_TST_POS = NA)

df2y <- bind_rows(df2x, df_dummy)
  

####============ Vectors used ==============================

q_vec <- sort(unique(df2y$timept))[3:6]
agesexvec <- sort(unique(df2y$agesex))

q_agesex <- df2y %>% 
  group_by(agesex, timept) %>% 
  summarize(HTS_TST_POS = sum(HTS_TST_POS, na.rm=T),
            TX_NEW = sum(TX_NEW, na.rm=T)) %>% 
  ungroup() %>% 
  gather(indic, val, HTS_TST_POS,TX_NEW) %>% 
  mutate(colvar = paste(indic, timept, sep="_")) %>% 
  select(-indic, -timept) %>% 
  spread(colvar, val)


####============ Vectors used ==============================

# Run function 
finallist <- list()
finallist <- map(.x = agesexvec[7:8], .f = ~abm_agesex(.x))

combined_df <- dplyr::bind_rows(finallist)

saveRDS(combined_df, "abm_output.rds")


