beg<-Sys.time()
# extract buffers for occurrence data and compute some summary stats
library(raster) # raster data
rasterOptions(maxmemory = 1e+09)
library(tidyverse)
# library(rgdal) # works with a library on machine to crunch data
# library(gdalUtils) # more gdal fucntionality
library(furrr) # if paralllellizing
library(tictoc)
library(sf) #vector data
# library(tigris) #county data
# options(tigris_use_cache = TRUE) # guessing this allows for clever usage of the data.
# # Projection for rasters
my_pr<- "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"

# see what I can see with this NLCD data so far

nlcd <- list.files("data/GIS_downloads/NLCD_wmgCNFPzEKD2TBCTk1Kl/", pattern = "*tiff$", full.names = T)
nlcd_stack<-raster::stack(nlcd)



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
bounds<-raster::extent(matrix(c(min(localities$longitude)
                                , min(localities$latitude)
                                , max(localities$longitude)
                                , max(localities$latitude)), nrow = 2))

rm(withstats2)

###################################
# # get boundaries for each county in MD, DE, PA, VA, WV
# statbord<-c("MD", "DE", "PA", "VA", "WV")
# 
# # reproject to match rasters
# allco<-st_as_sf(map_dfr(statbord, function(co){
#   counties(co, resolution = "5m") %>% 
#     st_transform(crs = st_crs(my_pr))
# })) 



# my_ext<-extent(allco)


sfed<-st_as_sf(localities
               , coords = c('longitude', 'latitude')
               , crs = "EPSG:4326") %>% 
  st_transform(crs = st_crs(my_pr)) 


tic()
nlcd_points<-raster::extract(nlcd_stack, sfed)
toc()

colnames(nlcd_points)<-gsub("NLCD_", ""
                            , gsub("_L48_.*", "", colnames(nlcd_points) ))

# tomin<-function(x){do.call(pmin.int, c(x, na.rm=TRUE))}



# CHELSA back in this file
#make a raster stack
bc <- raster::stack(list.files("data/fromR/lfs/current/", full.names = T))

# add bioclimatic value for each observation

#shrink the rasters
focused<-raster::crop(bc, bounds)

# get the data
chelsa_matrix<-data.frame(raster::extract(focused, localities))
names(chelsa_matrix)<-sapply(0:19, function(x)paste0("bioclim", x))
chelsa_points<-bind_cols(localities, chelsa_matrix[,2:20], data.frame(apply(nlcd_points, 2, as.character)))


# correlations not super low, deal with later
# try_cors<-cor(chelsa_matrix, use = "na.or.complete")

# dcors<-lower.tri(try_cors)
# high_cors<-which(abs(dcors*try_cors)>0.7, arr.ind =T)
# high_cors

# # select maximum number of variables without getting collinearity
# future::plan(strategy = "multiprocess", workers = 4)
# 
# min_vars <- future_map(15:13, function(nvars){
#   combo = combn(19, nvars, simplify = F)
#   big_list = map(combo, function(chosen){
#     submat = apply(chelsa_matrix[,c(chosen)], 2, as.numeric)
#     try_cors = cor(submat, use = "complete.obs")
#     # print(try_cors)
#     dcors = lower.tri(try_cors)
#     high_cors = which(abs(dcors*try_cors)>0.7, arr.ind =T)
#     if_else(length(high_cors)==0, return(list(combo, try_cors)), return(NULL))
#   })
#   sum_cors = # ifelse(is.null(big_list), big_list, 
#                     map(big_list, function(this_combo){
#     sc = sum(abs(this_combo[[2]]), na.rm =T)
#     mc = sum(abs(this_combo[[2]])>0.7)
#     # print(sc)
#     return(c(sc, mc))
#   })#)
#   winner = base::which.min(sapply(sum_cors, rbind)[1,])
#   # print(sum_cors[[winner]])
#   return(list(winner
#               , sum_cors = sapply(sum_cors, rbind)[1,winner]
#               , hc = sapply(sum_cors, rbind)[2,winner]
#               , chels_red = ifelse(is.null(big_list[[winner]])
#                                    , NA
#                                    , big_list[[winner]])))
# })
# 
# just_good<-sapply(min_vars, function(x){x[which(x$hc ==0)]})
# rm(bc)
# rm(focused)
# rm(bounds)
# rm(localities)
# gc()
# # do I have slope data?


slope<-raster("data/GIS_downloads/slope.tif")

pslope<-projectRaster(slope, crs = my_pr)

slope_points<-raster::extract(pslope, sfed)

# merge chelsa with occurrence
# clim_stat <- left_join(good_coords, chelsa_points
#                        , by = c("decimalLatitude" = "latitude"
#                                 , "decimalLongitude" = "longitude"))
# just_good<-sapply(min_vars, function(x){x[which(x$hc ==0)]})

# putting all the data together
# climUseStat<-left_join(clim_stat %>%
#                          rename(latitude = decimalLatitude
#                                 , longitude = decimalLongitude)
#                        , bind_cols(localities, lulc2010))

mysf<-function(x){st_as_sf(x
                           , coords = c('longitude', 'latitude')
                           , crs = "EPSG:4326") %>% 
    st_transform(crs = st_crs(my_pr)) }

chel_sf<-mysf(chelsa_points) %>% 
  mutate(slope = slope_points) 

obs<-st_as_sf(good_coords %>% select(lon = decimalLongitude, lat = decimalLatitude, roundedSRank, roundedNRank, roundedGRank, genus, species, exotic = exotic...17)
         , coords = c("lon",  "lat")
         , crs = "EPSG:4326") %>% 
  st_transform(crs = st_crs(my_pr)) 


indi<-st_join(chel_sf, obs)
save(indi, file="data/fromR/to_predict.RDA")
took<-Sys.time()-beg
took

# this is how to remove temp files if needed
# removeTmpFiles()
