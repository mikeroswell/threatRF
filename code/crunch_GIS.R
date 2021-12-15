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
library(tigris) #county data
options(tigris_use_cache = TRUE) # guessing this allows for clever usage of the data.
# Projection for rasters
my_pr<- "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"

# see what I can see with this NLCD data so far

nlcd <- list.files("data/GIS_downloads/NLCD_wmgCNFPzEKD2TBCTk1Kl/", pattern = "*tiff$")
nlcd

nlcd_stack<-raster::stack(
  c(sapply(nlcd, FUN = function(x){paste0("data/GIS_downloads/NLCD_wmgCNFPzEKD2TBCTk1Kl/", x)}))
)


raster::projection(nlcd_stack)


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
# get boundaries for each county in MD, DE, PA, VA, WV
statbord<-c("MD", "DE", "PA", "VA", "WV")

# reproject to match rasters
allco<-st_as_sf(map_dfr(statbord, function(co){
  counties(co, resolution = "5m") %>% 
    st_transform(crs = st_crs(my_pr))
})) 



# my_ext<-extent(allco)



####################################
# load the land use 2013 dataset, for now this is a test of workflow


# LU2013_layrs <- list.dirs("data/GIS_downloads/LU2013")[-1]
# 
# LU_tifs <- unlist(map(LU2013_layrs, function(dr){
#   list.files(dr, pattern = "*LandUse.tif$")
# }))
# 
# LU_tifs_full<-unlist(map(1:length(LU2013_layrs), function(county){
#   paste0(LU2013_layrs[county], "/", LU_tifs[county])
# }))
# 
# 
# # do similar for the land cover
# LC2013_layrs <- list.dirs("data/GIS_downloads/LC2013")
# 
# LC_tifs <- unlist(map(LC2013_layrs, function(dr){
#   list.files(dr, pattern = "*.img$")
# }))
# 
# 
# LC_tifs <- unlist(map(LC2013_layrs, function(dr){
#   list.files(dr, pattern = ".img$", full.names =T)
# }))
# 


# # make a key of counties with GEOID to match the rasters. This should work for
# # the 2013 LU rasters, not sure if it will also work for LC.
# 
# cokey<-data.frame(LU_tifs) %>% 
#   separate(LU_tifs, into= c("first_four", "GEOID", "crap"), sep = "_") %>% 
#   rownames_to_column(var="rasterID")
# 
# # add the new data back to allco
# allco<-allco %>% left_join(cokey)

# determine which county each point sits in (no buffer)
sfed<-st_as_sf(localities
               , coords = c('longitude', 'latitude')
               , crs = "EPSG:4326") %>% 
  st_transform(crs = st_crs(my_pr)) 

# # this takes ~4 seconds on laptop
# tic()
# withco<-st_join(sfed, allco)
# toc()


# # try adding buffer!
# # about 6 secs on laptop
# tic()
# mybufs<-st_buffer(sfed, dist = 1000)
# toc()

# # about 35 seconds on laptop
# tic()
# bufcos<-st_intersects(mybufs, allco)
# toc()
# 
# co_combs<-unique(bufcos) # unique combinations of counties



# tic()
# rerast<-map(LU_tifs_full, function(lyr){
#   rast = raster(lyr)
#   if(projection(rast)==my_pr){
#     return(rast)
#   }
#   else{print(paste(lyr, "has messed up projection info, it is", projection(rast), "should be", my_proj))}
#   # if getting different values for points based on poorly clipped rasters, may need to get the rasters to crop by the borders in `statbord`
# })
# toc()
# 
# LC_rast<-map(LC_tifs, function(rfile){
#   print(rfile)
#   raster(rfile)
# })

tic()
nlcd_points<-raster::extract(nlcd_stack, sfed)
toc()
nlcd_point<-nlcd_points
str(nlcd_point)
colnames(nlcd_point)
colnames(nlcd_point)<-gsub("NLCD_", ""
                            , gsub("_L48_.*", "", colnames(nlcd_point) ))
nlcd_point

tomin<-function(x){do.call(pmin.int, c(x, na.rm=TRUE))}


# CHELSA back in this file
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


# this loads the shape file into R's brain
lulc_years<-c(1973, 2002, 2010)

lulc_shape <- map(lulc_years, function(yr){
  read_sf(paste0("data/GIS_downloads/LULC", yr, "_unzipped/")) %>% 
    st_transform(crs = my_pr)
})
# there is a special data format for coordinates


# this took a while, but it is extracting the land use categorization associated with each point. Should write down how long it actually takes

# use st join for this
old_LULC<-map(lulc_shape, function(yr){
  st_join(sfed, yr) %>% select(-c( "COUNTY"
                                  , "ShapeSTAre"
                                  , "ShapeSTLen" ))
})


# do I have slope data?


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

chel_sf<-st_as_sf(chelsa_points
         , coords = c('longitude', 'latitude')
         , crs = "EPSG:4326") %>% 
  st_transform(crs = st_crs(my_pr)) 

chel_LU<-chel_sf %>% mutate(LU2013_points, LC2013_points
                            , slope = slope_points) 
alldat<-chel_LU %>% st_join(old_LULC[[1]]) %>% st_join(old_LULC[[2]]) %>% st_join(old_LULC[[3]] %>% filter(!is.na(OBJECTID)))
str(alldat)

nona<-alldat %>% drop_na() %>% st_as_sf()

obs<-st_as_sf(good_coords %>% select(lon = decimalLongitude, lat = decimalLatitude, roundedSRank, roundedNRank, roundedGRank, genus, species, exotic = exotic...17)
         , coords = c("lon",  "lat")
         , crs = "EPSG:4326") %>% 
  st_transform(crs = st_crs(my_pr)) 


indi<-st_join(nona, obs)
save(indi, file="data/fromR/to_predict.RDA")
took<-Sys.time()-beg
took

# this is how to remove temp files if needed
# removeTmpFiles()
