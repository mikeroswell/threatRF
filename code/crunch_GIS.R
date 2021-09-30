# extract buffers for occurrence data and compute some summary stats
library(raster) # raster data
rasterOptions(maxmemory = 1e+09)
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

# reproject to match rasters
allco<-st_as_sf(map_dfr(statbord, function(co){
  counties(co, resolution = "5m") %>% 
    st_transform(crs = st_crs(my_pr))
})) 



my_ext<-extent(allco)
# extr<-extend(rerast[[1]], extent(allco))

par(mfrow(c(1,2)))
plot(extr)
plot(rerast[[1]])
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


# do similar for the land cover
LC2013_layrs <- list.dirs("data/GIS_downloads/LC2013")
LC2013_trim<-LC2013_layrs[grepl("data/GIS_downloads/LC2013/.*/.*.", LC2013_layrs)]
LC2013_trim

# LC_test<-raster::raster("data/GIS_downloads/LC2013/ALLE_24001/ALLE_24001/ALLE_24001.img")


LC_tifs <- unlist(map(LC2013_layrs, function(dr){
  list.files(dr, pattern = "*.img$")
}))

LC_tifs_full<-unlist(map(1:length(LC2013_trim), function(county){
  paste0(LC2013_trim[county], "/", LC_tifs[county])
}))

LC_rast<-map(LC_tifs_full[1:24], function(rfile){
  print(rfile)
  raster(rfile)
})


# do similar for the land cover
LC2013_layrs <- list.dirs("data/GIS_downloads/LC2013")


LC_test<-raster::raster("data/GIS_downloads/LC2013/ALLE_24001/ALLE_24001/ALLE_24001.img")

LC_tifs <- unlist(map(LC2013_layrs, function(dr){
  list.files(dr, pattern = ".img$", full.names =T)
}))



LC_rast<-map(LC_tifs, function(rfile){
  print(rfile)
  raster(rfile)
})



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
plan(strategy = "multiprocess", workers = 3)
tic()
future_map(LU_tifs_full, function(lyr){
  rast = raster(lyr)
  if(projection(rast)==my_pr){
    writeRaster(extend(rast, extent(allco)), filename = lyr, overwrite = T)
  }
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

plan(strategy = "multiprocess", workers = 6)
tic()
LC_extraction<-future_map(LC_rast, .options = furrr_options(packages = "sf"), function(co){
  raster::extract(co, sfed)
})
toc()

tic()
trystack<-raster::stack(rerast, tolerance =0.5)
toc()



tomin<-function(x){do.call(pmin.int, c(x, na.rm=TRUE))}



LC_reduction<-tomin(LC_extraction)
LU_reduction <- tomin(LU_extraction)
# <2 min

per_co<-map(LU_extraction, function(co){
  sum(complete.cases(co))
})


sum(unlist(per_co)) # actually a bit bigger than total number of points, i.e. either some points fall into multiple county rasters or an error. Ignore for now. 

# try again to make a mosaic with all the points
moser <- function(rast_list, tolerance, funlist){
  rast_list$tolerance = tolerance
  # rast_list$na.rm =T
  rast_list$fun = funlist
  do.call(mosaic, rast_list)
}
# made the moser equation maybe simpler, testing again
tic()
mosTest<-moser(rast_list = c(rerast[[1]], rerast[[2]]), tolerance = 0.5, funlist = "min")
toc()

#going nowhere on this so far.
tic()
big_mos<-moser(rast_list = rerast, tolerance = 0.5, funlist = "min")
toc()
# do I need merge?
tic()
test_merge <- merge(rerast[[1]], rerast[[2]], tolerance = 0.5, fun = "min")
toc()
# multco<-c(3,5)
# 
# allco[c(multco),]
# 
# 
# # this is working to make the mosaic. Setting tolerance quite high seems to be the trick. 
tic()
test_mos<-mosaic(rerast[[1]], rerast[[2]], tolerance = 0.5, fun = "min")
toc()

tic()
crop_first<-raster::stack(raster::extend(rerast[[1]], raster::extent(allco)), raster::extend(rerast[[2]], raster::extent(allco)), tolerance = 0.5, fun = "min" )
toc()

# 31037.159 sec elapsed
# that's a long long time. Yikes.
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




# get number of t