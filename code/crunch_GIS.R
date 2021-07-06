# extract buffers for occurrence data and compute some summary stats
library(raster)
library(tidyverse)


good_coords<- withstats2 %>%
  filter(decimalLatitude<44 & decimalLatitude> 34 &decimalLongitude>-82 & decimalLongitude < -73) # some errors, check workflow that they weren't introduced here.

#df of lat and long
localities <- good_coords %>%
  group_by(decimalLatitude, decimalLongitude) %>%
  summarize(n()) %>%
  select( longitude = decimalLongitude, latitude = decimalLatitude)


#crop the rasters based on extent
bounds<-raster::extent(matrix(c(min(localities$longitude), min(localities$latitude), max(localities$longitude), max(localities$latitude)), nrow = 2))



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