# extract buffers for occurrence data and compute some summary stats
library(raster) # raster data
library(tidyverse)
# library(rgdal) # works with a library on machine to crunch data
# library(gdalUtils) # more gdal fucntionality
library(furrr) # if paralllellizing
library(tictoc)
library(sf) #vector data
library(tigris) #county data
options(tigris_use_cache = TRUE) # guessing this allows for clever usage of the data.
# Projection for rasters
my_pr<- "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"

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
# bounds<-raster::extent(matrix(c(min(localities$longitude), min(localities$latitude), max(localities$longitude), max(localities$latitude)), nrow = 2))

rm(withstats2, good_coords)

###################################
# get boundaries for each county in MD, DE, PA, VA, WV
statbord<-c("MD", "DE", "PA", "VA", "WV")

# reprojec to match rasters
allco<-st_as_sf(map_dfr(statbord, function(co){
  counties(co, resolution = "5m") %>% 
    st_transform(crs = st_crs(my_pr))
})) 

# make a key of counties with GEOID to match the rasters. This should work for
# the 2013 LU rasters, not sure if it will also work for LC.

####################################
# load the land use 2013 dataset, for now this is a test of workflow
LU2013_layrs <- list.dirs("data/GIS_downloads/LU2013")[-1]

LU_tifs <- unlist(map(LU2013_layrs, function(dr){
  list.files(dr, pattern = "*LandUse.tif$")
}))

LU_tifs_full<-unlist(map(1:length(LU2013_layrs), function(county){
  paste0(LU2013_layrs[county], "/", LU_tifs[county])
}))



cokey<-data.frame(LU_tifs) %>% 
  separate(LU_tifs, into= c("first_four", "GEOID", "crap"), sep = "_") %>% 
  rownames_to_column(var="rasterID")

# add the new data back to allco
allco<-allco %>% left_join(cokey)

# determine which county each point sits in (no buffer)
sfed<-st_as_sf(localities
               , coords = c('longitude', 'latitude')
               , crs = "EPSG:4326") %>% 
  st_transform(crs = st_crs(my_pr)) 

# this takes ~4 seconds on laptop
tic()
withco<-st_join(sfed, allco)
toc()


# try adding buffer!
# about 6 secs on laptop
tic()
mybufs<-st_buffer(sfed, dist = 1000)
toc()

# about 35 seconds on laptop
tic()
bufcos<-st_intersects(mybufs, allco)
toc()

co_combs<-unique(bufcos) # unique combinations of counties


#this all seems like it is working. maybe map to see if it's right!
# ggplot(allco) +
#   geom_sf() +
#   theme_classic() +
#   geom_sf(data = mybufs, color = "red")

# looks basically right. There are some weird points that are very far from the state of Maryland, and also some that appear to be slightly outside. I'm not going to worry about it for now, as I'm pretty fired up that this is working at all!


# this is where the LU tifs actually get loaded into R's brain
tic()
rerast<-map(LU_tifs_full, function(lyr){
  rast = raster(lyr)
  if(projection(rast)==my_pr){rast}
  else{print(paste(lyr, "has messed up projection info, it is", projection(rast), "should be", my_proj))}
  # if getting different values for points based on poorly clipped rasters, may need to get the rasters to crop by the borders in `statbord`
})
toc()

# extract points (this is simple)
plan(strategy = "multiprocess", workers = 6)
tic()
LU_extraction<-future_map(rerast, .options = furrr_options(packages = "sf"), function(co){
  raster::extract(co, sfed)
})
toc()

# <2 min

per_co<-map(LU_extraction, function(co){
  sum(complete.cases(co))
})


sum(unlist(per_co)) # actually a bit bigger than total number of points, i.e. either some points fall into multiple county rasters or an error. Ignore for now. 

# multco<-c(3,5)
# 
# allco[c(multco),]
# 
# 
# # this is working to make the mosaic. Setting tolerance quite high seems to be the trick. 
# test_mos<-mosaic(rerast[[1]], rerast[[2]], tolerance = 0.5, fun = "min")
# 
# 
# allco$COUNTYFP

# plan(strategy = "multiprocess", workers = 4)
tic()
co_mos <- map(co_combs
                     # , .options = furrr_options(packages = "sf")
                     , function(multco){
                       if(length(multco)>0){
                        rasters = allco[multco,]$rasterID
                        if(length(rasters)>0){
                          comprast = rasters[complete.cases(rasters)]
                          if(length(comprast)>1){
                             rlist = rerast[as.numeric(comprast)]
                             rlist$fun= min
                             rlist$na.rm = t
                             rlist$tolerance = 0.5
                             tryCatch(do.call(mosaic, rlist), error=function(e){print(paste("this combo failed", multco))})
                           }
                           
                           if(length(comprast)==1){
                             rerast[[comprast]]}
                        }
                        else{print(multco)
                          return(list())}
                       }
                       
                       else{print(multco)
                         return(list())}
                     })
toc()

# this was a plot for a single county
# pdf("figures/test_reproj.pdf")
# rasterVis::gplot(first_reproj)+
#   geom_tile(aes(fill=value))+
#   theme_classic() +
#   layer_spatial(sped)
# # xlim(-80, -72) +
# # ylim(32, 45)
# dev.off()


# this just plots the rasters, no points
# pdf("figures/testmap.pdf")
# future_map(rerast, function(co){
#   rasterVis::gplot(co)+
#     geom_tile(aes(fill=factor(value)))+
#     theme_classic() +
#     xlim(-80, -72) +
#     ylim(32, 45)
# })
# dev.off()




# get number of types in 1 km buffer
# this won't work as written becuase some buffers fall outside counties, need to get the buffer script working first.
head(bufcos)
str(bufcos)

# plan(strategy = "multiprocess", workers=5)

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
# future::plan(strategy = "multiprocess", workers = 7)

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