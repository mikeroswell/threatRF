# extract buffers for occurrence data and compute some summary stats
library(raster) # raster data
library(tidyverse)
library(rgdal) # works with a library on machine to crunch data
library(gdalUtils) # more gdal fucntionality
library(furrr) # if paralllellizing
library(tictoc)
library(sf) #vector data
library(tigris) #county data
options(tigris_use_cache = TRUE) # guessing this allows for clever usage of the data.


# get the occcurence data
withstats2 <- read.csv("data/fromR/lfs/plants_with_status.csv")
# get the coordinates (may require additional manipulation)
good_coords<- withstats2 %>%
  filter(decimalLatitude<44 & decimalLatitude> 34 &decimalLongitude>-82 & decimalLongitude < -73) # some errors, check workflow that they weren't introduced here.

#df of lat and long
localities <- good_coords %>%
  group_by(decimalLatitude, decimalLongitude) %>%
  summarize(n()) %>%
  dplyr::select( longitude = decimalLongitude, latitude = decimalLatitude)

# localities
#crop the rasters based on extent
bounds<-raster::extent(matrix(c(min(localities$longitude), min(localities$latitude), max(localities$longitude), max(localities$latitude)), nrow = 2))

rm(withstats2, good_coords)

###################################
# get boundaries for each county in MD, DE, PA, VA, WV
statbord<-c("MD", "DE", "PA", "VA", "WV")
allco<-st_as_sf(map_dfr(statbord, function(co){
  counties(co, resolution = "5m")
})) # not sure what the projection is here
# determine which county each point sits in (no buffer)
sfed<-st_as_sf(localities, coords = c('longitude', 'latitude'), crs = st_crs(allco)) %>% st_transform(4326) #transform to NAD83(NSRS2007) / California Albers
withco<-st_join(sfed, allco %>% st_transform(4326))
# try adding buffer!
# this takes<6 seconds on laptop

mybufs<-st_buffer(sfed, dist = 1000)

tic()
bufcos<-st_intersects(mybufs, allco %>% st_transform(4326))
toc()
# 37 seconds

co_combs<-unique(bufcos) # unique combinations of counties


#this all seems like it is working. maybe map to see if it's right!
# ggplot(allco) +
#   geom_sf() +
#   theme_classic() +
#   geom_sf(data = mybufs, color = "red")

# looks basically right. There are some weird points that are very far from the state of Maryland, and also some that appear to be slightly outside. I'm not going to worry about it for now, as I'm pretty fired up that this is working at all!


####################################
# start computing 1 km buffers
LU2013_layrs <- list.dirs("data/GIS_downloads/LU2013")[-1]

LU_tifs <- unlist(map(LU2013_layrs, function(dr){
  list.files(dr, pattern = "*LandUse.tif$")
}))

LU_tifs_full<-unlist(map(1:length(LU2013_layrs), function(county){
  paste0(LU2013_layrs[county], "/", LU_tifs[county])
}))
# LU_tifs_full

# this looks like it's a nope. Too much data for the laptop
# #create a blank canvas
# writeRaster(raster(bounds), "data/GIS_downloads/LU_combined.tif", format = "GTiff", overwrite =T)

# # paste all the files into it
# mosaic_rasters(gdalfile = LU_tifs_full
#                , dst_dataset = "data/GIS_downloads/LU_combined.tif"
#                , of = "GTiff" )
# bigLU<-mosaic(gdalfile = LU_tifs_full, dst_dataset = "data/GIS_downloads/LU_combined.tif")

rerast<-map(LU_tifs_full, function(lyr){
  rast = raster(lyr)
  raster::crs(rast) <- "EPSG:4326"
  # extent(rast)<-extent(bounds)
  return(rast)
})

# # rerast
# ?raster::mosaic
# tic()
# try_mos<-do.call(raster::mosaic, rerast)
# toc()
#
# # from https://stackoverflow.com/a/15306786/8400969
# setMethod('mosaic', signature(x='list', y='missing'),
#           function(x, y, fun, tolerance=0.1, filename=""){
#             stopifnot(missing(y))
#             args <- x
#             if (!missing(fun)) args$fun <- fun
#             if (!missing(tolerance)) args$tolerance<- tolerance
#             if (!missing(filename)) args$filename<- filename
#             do.call(mosaic, args)
#           })
#
# low_tolerance <- raster::mosaic(rerast, tolerance =0.1)
# apparently have different resolutions to deal with here.

# do only the points (more data rich)
sped<-as(sfed, "Spatial") # this is a spatial points dataframe

plan(strategy = "multiprocess", workers = 6)

tic()
landUsePoints_sf<-future_map(rerast, function(county){
  if(compareCRS(county, sped)){raster::extract(county, sped)}
  else{return("check CRS")}
})
toc()

sum(complete.cases(landUsePoints_sf))
# now 2 seconds.

plot(rerast[[1]])



pdf("figures/testmap.pdf")
future_map(rerast, function(co){
  rasterVis::gplot(co)+
    geom_tile(aes(fill=factor(value)))+
    theme_classic() +
    xlim(-80, -72) +
    ylim(32, 45)
})
dev.off()



# 1009 seconds on laptop most recently
landUsePoints # hooray, this seems to work, double check and get good info
sum(is.na(landUsePoints[[1]]))
landUsePoints[[1]]+landUsePoints[[2]]


asSum<-rowSums(do.call(cbind, landUsePoints), na.rm =T)
asMax<-apply(do.call(cbind,landUsePoints),1, function(x){max(x, na.rm =T)})
all.equal(asMax, asSum)
# <10 sec on most recent run on amarel, but returned only NAs
# this is where I should focus on troubleshooting next.

# get number of types in 1 km buffer
# this won't work as written becuase some buffers fall outside counties, need to get the buffer script working first.

plan(strategy = "multiprocess", workers=5)

tic()
landUseTypes1Km<-map(rerast, function(county){
  raster::extract(county, localities
                  , buffer =1000
                  , fun = length
  )
})
toc()

# running into serious memory issues
tic()
first_co<-raster::extract(rerast[[1]], localities, buffer = 1000, fun = function(x){length(unique(x))})
toc()


# bigLU_stack<-raster::stack(rerast, quick = T)
# see if slope data is ccredible
alle_slope<-raster("data/GIS_downloads/Allegany_slope.tiff")
# produces error, cooncerning:
slope_cropped<-raster::crop(alle_slope, bounds)
target_slope<-data.frame(raster::extract(alle_slope, data.frame(ungroup(localities))))
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

rm(bc)
rm(focused)
rm(bounds)
rm(localities)
gc()
# correlations not super low, deal with later
# try_cors<-cor(chelsa_matrix, use = "na.or.complete")

# dcors<-lower.tri(try_cors)
# high_cors<-which(abs(dcors*try_cors)>0.7, arr.ind =T)
# high_cors

# select maximum number of variables without getting collinearity
future::plan(strategy = "multiprocess", workers = 7)

min_vars <- map(19:18, function(nvars){
  combo = combn(19, nvars, simplify = F)
  big_list = map(combo, function(chosen){
    submat = apply(chelsa_matrix[,c(chosen)], 2, as.numeric)
    try_cors = cor(submat, use = "complete.obs")
    # print(try_cors)
    dcors = lower.tri(try_cors)
    high_cors = which(abs(dcors*try_cors)>0.7, arr.ind =T)

    if_else(!length(high_cors)>0, return(list(combo, try_cors)), return(NULL))
  })
  sum_cors = map(big_list, function(this_combo){
    sc = sum(abs(this_combo[[2]]), na.rm =T)
    # print(sc)
    return(sc)
  })
  winner = base::which.min(sum_cors)
  # print(sum_cors[[winner]])
  return(list(winner, sum_cors= sum_cors[[winner]], big_list[[winner]]))
})

min_vars[[1]]

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