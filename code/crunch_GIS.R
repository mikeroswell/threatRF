# extract buffers for occurrence data and compute some summary stats
library(raster)
library(tidyverse)
library(rgdal)
library(gdalUtils)

# get the occcurence data
withstats2 <- read.csv("data/fromR/lfs/plants_with_status.csv")
# get the coordinates (may require additional manipulation)
good_coords<- withstats2 %>%
  filter(decimalLatitude<44 & decimalLatitude> 34 &decimalLongitude>-82 & decimalLongitude < -73) # some errors, check workflow that they weren't introduced here.

#df of lat and long
localities <- good_coords %>%
  group_by(decimalLatitude, decimalLongitude) %>%
  summarize(n()) %>%
  select( longitude = decimalLongitude, latitude = decimalLatitude)

localities
#crop the rasters based on extent
bounds<-raster::extent(matrix(c(min(localities$longitude), min(localities$latitude), max(localities$longitude), max(localities$latitude)), nrow = 2))

####################################
# start computing 1 km buffers
LU2013<-readOGR("data/GIS_downloads/LU2013/ALLE_24001_LandUse/")
alle_LU<-raster("data/GIS_downloads/LU2013/ALLE_24001_LandUse/ALLE_24001_LandUse.tif")
LU2013_layrs<-list.dirs("data/GIS_downloads/LU2013")[-1]
LU_tifs<-unlist(map(LU2013_layrs, function(dr){
  list.files(dr, pattern = "*LandUse.tif$")
}))
LU_tifs_full<-unlist(map(1:length(LU2013_layrs), function(county){
  paste0(LU2013_layrs[county], "/", LU_tifs[county])
}))
LU_tifs_full

#create a blank canvas
writeRaster(raster(bounds), "data/GIS_downloads/LU_combined.tif", format = "GTiff")
# paste all the files into it
mosaic_rasters(gdalfile = LU_tifs_full, dst_dataset = "data/GIS_downloads/LU_combined.tif", of = "GTiff")




# see if slope data is ccredible
alle_slope<-raster("data/GIS_downloads/Allegany_slope.tiff")
# produces error, cooncerning:
slope_cropped<-raster::crop(alle_slope, bounds)
target_slope<-data.frame(raster::extract(alle_slope, localities))
plot(alle_slope)
summary(target_slope)

extent(alle_slope)
extent(localities)
###############################
# extract bioclimatic variables

# first create stack of the 19 layers
# then crop to match occurrence extents
# then extract value at leach locale
# finally (not yet implemented) eliminate variables to reduce collinearity

#make a raster stack
bc <- raster::stack(list.files("data/fromR/lfs/current/", full.names = T))

# add bioclimatic value for each observation

#shrink the rasters
focused<-raster::crop(bc, bounds)

# get the data
chelsa_matrix<-data.frame(raster::extract(focused, localities))
names(chelsa_matrix)<-sapply(1:19, function(x)paste0("bioclim", x))
chelsa_points<-bind_cols(localities, chelsa_matrix)

# correlations not super low, deal with later
try_cors<-cor(chelsa_matrix, use = "na.or.complete")

dcors<-lower.tri(try_cors)
high_cors<-which(abs(dcors*try_cors)>0.7, arr.ind =T)
high_cors


# merge chelsa with occurrence
clim_stat <- left_join(good_coords, chelsa_points
                       , by = c("decimalLatitude" = "latitude"
                                , "decimalLongitude" = "longitude"))
# this loads the shape file into R's brain
lulc_shape<-readOGR("data/LULC_2010/")
# there is a special data format for coordinates
ll<-SpatialPoints(localities)
# and this data format has attributes related to projection etc. that needs to match across layers
proj4string(lulc_shape)
proj4string(ll)<-proj4string(lulc_shape)

# this took a while, but it is extracting the land use categorization associated with each point. Should write down how long it actually takes
tictoc::tic()
lulc2010<-sp::over(ll, lulc_shape)
tictoc::toc()

# putting all the data together
climUseStat<-left_join(clim_stat %>%
                         rename(latitude = decimalLatitude
                                , longitude = decimalLongitude)
                       , bind_cols(localities, lulc2010))

# names(clim_stat)<-make.names(clim_stat)
# write data to .csv
data.table::fwrite(climUseStat[,-172], "data/fromR/lfs/plants_1989-2019_with_status_climate_landUse.csv", row.names =F)