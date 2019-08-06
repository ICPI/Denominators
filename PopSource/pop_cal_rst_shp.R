#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Population Calculation (using Shape and raster file) - function ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
install.packages("rgdal")
install.packages("raster")

library(rgdal)
library(raster)

# Creating function for extracting population using Ratser files and Shape files
# Inputs to the fuinction - shp_file (.shp), rst_file (.tif)

pop_function <- function(shp_file, rst_file)
{
  # Craete a column for saving population numbers
  shp_file@data$pop <- NA
  
  # Loop for sub national regions
  for (i in 1:nrow(shp_file@data))
  {
    r2 <- crop(rst_file, extent(shp_file[i,])) # returnes raster with extreme N-S-E-West of psn shapefile (rectangular extent)
    r2_pop <- mask(r2, shp_file[i,])  # create raster with the same geometry of the shapefile
    
    shp_file@data$pop[i]<-cellStats(r2_pop,stat="sum",na.rm=T)
    #print(i)
    #print(shp_file@data$pop[i])
  }  
  return(shp_file@data)
}