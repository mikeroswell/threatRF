beg<-Sys.time()
# extract buffers for occurrence data and compute some summary stats
# did raster used to import data differently?
library(raster) # raster data
rasterOptions(maxmemory = 1e+09)
library(tidyverse)
`%ni%` <- Negate(`%in%`) #convenience, this should be part of base R!
# library(rgdal) # works with a library on machine to crunch data
# library(gdalUtils) # more gdal fucntionality
library(furrr) # if paralllellizing
library(tictoc)
library(sf) #vector data
library(rnaturalearth)
library(tigris) #county data
options(tigris_use_cache = TRUE) # guessing this allows for clever usage of the data.
# # Projection for rasters
my_pr<- "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"


# get the occcurrence data
# pstats <- read.csv("data/fromR/lfs/kept_plants_with_stats.csv") %>%
#   mutate(taxon = "plant") %>% 
#   mutate(georeferencedDate = as.character(georeferencedDate)
#          , datasetID = as.character(datasetID))
# lstats <- read.csv("data/fromR/lfs/native_leps_with_status.csv") %>%
#   mutate(taxon = "lep") %>% 
#   mutate(georeferencedDate = as.character(georeferencedDate)
#          , datasetID = as.character(datasetID))
# # I think it will be fine to have columns from Knapp that are dropped in 
# # analysis... may need to adjust. 
# withstats2 <- bind_rows(pstats 
#                      , lstats)

withstats2 <- read.csv("data/fromR/lfs/native_records.csv")
# get the coordinates (may require additional manipulation)
good_coords <- withstats2 %>%
  filter(decimalLatitude<44 & decimalLatitude> 34 &decimalLongitude>-82 & decimalLongitude < -73) %>% 
  mutate(UID = rownames(.)) # some errors, check workflow that they weren't introduced here.

# exotic_records <- withstats2 %>%
#   filter(decimalLatitude<44 & decimalLatitude> 34 &decimalLongitude>-82 & decimalLongitude < -73) %>%
#   # grab only species listed as introduced
#   filter(exotic...175| exotic...173|exotic...170| exotic...172) %>%
#   mutate(UID = rownames(.)) # some errors, check workflow that they weren't introduced here.
# 
# write.csv(exotic_records, file = "data/fromR/lfs/exotic_records_removed.csv")
# drop non-MD points

#df of lat and long
localities <- good_coords %>%
  group_by(decimalLatitude, decimalLongitude) %>%
  summarize(n()) %>%
  dplyr::select( longitude = decimalLongitude, latitude = decimalLatitude)

# localities
#crop the rasters based on extent
bounds <- raster::extent(matrix(c(min(localities$longitude)
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

# get MD state boundary
MD_boundary <- counties("MD", resolution = "5m") %>% 
  st_union() %>% 
  st_transform(crs = st_crs(my_pr))
# check work
# ggplot() + geom_sf(data = MD_boundary, mapping=aes()
#                    , color="black"
#                    , fill = "white"
#                    , size=0.25)

sfed_all <- st_as_sf(localities
               , coords = c('longitude', 'latitude')
               , crs = "EPSG:4326") %>% 
  st_transform(crs = st_crs(my_pr))


keepInd <- unlist(st_contains(MD_boundary, sfed_all))

# this was wildly slow, and the line above fixed it
# sfed_MD <- sfed_all %>% mutate(in_MD = lengths(st_within(sfed_all, MD_boundary)))

# sfed <- sfed_MD %>% filter(in_MD ==1) %>% dplyr::select(-in_MD)


# 
# sfed_outside <- sfed_MD %>% filter(in_MD ==0) %>% dplyr::select (-in_MD)
sfed <- sfed_all[keepInd, ]
sfed_oputside <- sfed_all[-keepInd, ]

# see what I can see with this NLCD data so far

nlcd <- list.files("data/GIS_downloads/NLCD_wmgCNFPzEKD2TBCTk1Kl/"
                   , pattern = "*tiff$", full.names = T)
nlcd_stack<-raster::stack(nlcd)

# extract
# tic()
nlcd_points <- raster::extract(nlcd_stack, sfed)
# toc() 
# <9 sec

colnames(nlcd_points)<-gsub("NLCD_", ""
                            , gsub("_L48_.*", "", colnames(nlcd_points) ))

# tomin<-function(x){do.call(pmin.int, c(x, na.rm=TRUE))}



# CHELSA back in this file
#make a raster stack
# the "tcc" layers apparently came from a different resolution or something

# omitting a handful of variables 
bc <- raster::stack(list.files("data/GIS_downloads/CHELSA"
                               , full.names = TRUE)[c(1:62, 67:70)])
# just trusting skipping some in the 60s, after 70.
         

#shrink the rasters
focused<-raster::crop(bc, bounds)

# get the data
chelsa_matrix<-data.frame(raster::extract(focused, sfed))


names(chelsa_matrix)<-gsub("_1981.*", "", names(chelsa_matrix))

apply(chelsa_matrix, 2, function(x){length(unique(x))})
apply(chelsa_matrix, 2, function(x){sum(complete.cases(x))})

# set minimum number of complete cases
cc <- 41000
cat("You set the minimum number of complete cases as", cc)
# this was higher before. Not sure if this is too low now. none dropped.
drop_vars <- c(names(which(
  apply(chelsa_matrix, 2, function(x){sum(complete.cases(x))}) <= cc) )
    , names(which(apply(chelsa_matrix, 2, function(x){length(unique(x))}) < 4)))

write.csv(drop_vars, "data/fromR/CHELSA_variables_dropped.csv", row.names =F)



chelsa_complete <- chelsa_matrix[ 
  , intersect(which(apply(chelsa_matrix, 2, function(x){
    sum(complete.cases(x))
    }) > cc),
        which(apply(chelsa_matrix, 2, function(x){length(unique(x))}) > 3))
  ]



chelsa_points <- bind_cols(
  localities[keepInd,] # just coordinates?
  , chelsa_complete # CHELSA extended
  , data.frame(apply(nlcd_points, 2, as.character)) # LULC data
  )


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


slope <- raster("data/GIS_downloads/slope.tif")

pslope <- projectRaster(slope, crs = my_pr)

slope_points <- raster::extract(pslope, sfed)

mysf <- function(x){st_as_sf(x
                           , coords = c('longitude', 'latitude')
                           , crs = "EPSG:4326") %>% 
    st_transform(crs = st_crs(my_pr)) }

chel_sf <- mysf(chelsa_points) %>% 
  mutate(slope = slope_points) 


obs <- st_as_sf(good_coords %>% 
                dplyr::select(lon = decimalLongitude
                       , lat = decimalLatitude
                       , roundedSRank
                       , simple_status
                       , genus
                       , species
                       , kingdomKey
                       , UID)
         , coords = c("lon",  "lat")
         , crs = "EPSG:4326") %>% 
  st_transform(crs = st_crs(my_pr)) 

# merge CHELSA, occurrence with status, and other GIS covariates like slope and
# land use
indi <- st_join(chel_sf, obs)

# check conservation status of omitted observations
# good_coords %>% anti_join(indi) %>% mutate(nsp= n_distinct(species), nobs = n()) %>% 
#   group_by(roundedSRank) %>% 
#   summarize(spp = n_distinct(species), obs = n(), spProp = spp/mean(nsp), obsProp = obs/mean(nobs)) 
# 
# indi%>% mutate(nsp= n_distinct(species), nobs = n()) %>% 
#   group_by(roundedSRank) %>% 
#   summarize(spp = n_distinct(species), obs = n(), spProp = spp/mean(nsp), obsProp = obs/mean(nobs))
# looks like the dropped ones have S2, S4, S5 spp overrepresented, unranked under-represented
# I think this will be fine overall. 

#############################################
# remove non-native taxa

# nonNative <- read.csv("data/fromR/lfs/nonNative.csv")
# nn<- nonNative %>% filter(Region == "L48")
# head(nn)

save(indi, file="data/fromR/lfs/to_predict.RDA")
took<-Sys.time()-beg
took # under 2 min on new MBP. 

# # Some data checking. Grrrr
# 
# indi %>% sf::st_drop_geometry() %>% 
#   filter(roundedSRank == "S4" , simple_status == "threat") %>% 
#   group_by(genus, species, simple_status, roundedSRank) %>% 
#   summarize(n())


# # cleanup recommended; Tmp Files can get big.
# rm(list = ls())
# removeTmpFiles()
# gc()
