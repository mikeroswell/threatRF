# code to download gis data
library(tidyverse)
library(sp)
library(rgdal)
# raster has conflicts with dplyr.
# library(raster) #working with raster data (has fancy things to load parts of file, process, move on)
library(climatedata) #should be useful for downloading chelsa data programatically. Requires an update tho. Right now using chelsa v1 but should prob. use v2.
source("code/get_chelsa_revised.R") #hand-edited version of main function that works fine

# unique IDs for the layers I want
LULC1973<-"e98677701fb547a695b2f650bb19f35e"
LULC2002<-"96116be90edb4e8d933048f345c3a487"
LULC2010<-"97717f333baf4e79abb7ab8098a99ee5"
# parcels <- "042c633a05df48fa8561f245fccdd750" # no dice with this one
parcels <- "dc2d4fec9e814cb98b418babffec16a4"
slope <- "9268765c1c6e468c880e98482673de63" # no dice with this one

layers<- c("LULC2010", "LULC2002", "LULC2010", "parcels", "elevation")

getlayer <- function(x,y){
  download.file(paste0("http://data.imap.maryland.gov/datasets/", get(x), "_", y, ".zip")
                , paste0("data/GIS_downloads/", x)
  )
}
# http://data.imap.maryland.gov/datasets/96116be90edb4e8d933048f345c3a487_1.zip
# http://data.imap.maryland.gov/datasets/96116be90edb4e8d933048f345c3a487_1.zip
# http://data.imap.maryland.gov/datasets/97717f333baf4e79abb7ab8098a99ee5_0.zip
# http://data.imap.maryland.gov/datasets/042c633a05df48fa8561f245fccdd750_0.zip
# http://data.imap.maryland.gov/datasets/dc2d4fec9e814cb98b418babffec16a4_2.zip

map(layers, function(x){
  map(0:4, function(y){
    if(RCurl::url.exists(paste0("http://data.imap.maryland.gov/datasets/", get(x), "_", y, ".zip"))){
      getlayer(x, y)
      print (paste0("http://data.imap.maryland.gov/datasets/", get(x), "_", y, ".zip"))
    }
    else print(paste0("no url: ", paste0("http://data.imap.maryland.gov/datasets/", get(x), "_", y, ".zip")))
  })
})

# the first few parcel attempts failed, i think
# myshp<-rgdal::readOGR("/Users/mikeroswell/Downloads/Maryland_Property_Data_-_Parcel_Points.kml")
#
# download.file("https://geodata.md.gov/imap/rest/services/%20PlanningCadastre/MD_PropertyData/MapServer/exts/MDiMapDataDownload/customLayers/0", "data/GIS_downloads/parcel")

#slopes
download.file("https://lidar.geodata.md.gov/imap/rest/services/Statewide/MD_statewide_slope_m/ImageServer/exportImage?bbox=182999.96769999713,26372.607708967,570465.2458305534,233618.1246000008"
              , "data/GIS_downloads/slope")

#super high res rasters for Land Use from Chesapeake Conservancy
download.file("https://cicwebresources.blob.core.windows.net/cbp-1m-lu-2013/data/Maryland_1m_LU.zip"
              , "data/GIS_downloads/LU2013", timeout = 900) #longer timeout for huge files.

download.file("https://cicwebresources.blob.core.windows.net/chesapeakebaylandcover/MD/_MD_STATEWIDE.zip"
              , "data/GIS_downloads/LC2013", timeout = 900)


# unzip files

zippy<- c(layers, "LU2013", "LC2013")

map(zippy, function(fn){
  dir.create(paste0("data/GIS_downloads/", fn, "_unzipped" ))
  unzip(paste0("data/GIS_downloads/", fn), exdir = paste0("data/GIS_downloads/", fn, "_unzipped"))
  file.remove(paste0("data/GIS_downloads/", fn))
})

# chesapeake conservancy files

# Land Cover first
doc<-httr::GET("https://www.chesapeakeconservancy.org/conservation-innovation-center/high-resolution-data/land-cover-data-project/")

CIC<-"https://cicwebresources.blob.core.windows.net/chesapeakebaylandcover/MD/"
parsed<-XML::htmlParse(doc)
links <- XML::xpathSApply(parsed, "//a/@href")
todl<-links[grepl(CIC, links)]
fns<-gsub(CIC, "", todl)

dir.create("data/GIS_downloads/LC2013")
map(todl, function(landcover){
  short<-gsub(CIC, "", landcover)
  newfile<-paste0("data/GIS_downloads/LC2013/", short )
  tryCatch(download.file(landcover, destfile = newfile, timeout = 900), error= function(e){print(paste0(landcover, " did not download"))})
  if(file.exists(newfile)){
    unzip(newfile, exdir = gsub(".zip", "", newfile))
  }
  if(dir.exists(gsub(".zip", "", newfile))){
    file.remove(newfile)
    print(paste0("everything worked for ", short))
  }
})


# land use second

docLU<- httr::GET("https://www.chesapeakeconservancy.org/conservation-innovation-center/high-resolution-data/land-use-data-project/")

CICLU<-"https://cicwebresources.blob.core.windows.net/cbp-1m-lu-2013/data/"

parsedLU<-XML::htmlParse(docLU)
linksLU <- XML::xpathSApply(parsedLU, "//a/@href")
todlLU<-unlist(map(gsub(".zip", "", fns), function(counties){linksLU[grepl(counties, linksLU)]}))

dir.create("data/GIS_downloads/LU2013/")
map(todlLU, function(landuse){
  fn<-gsub(CICLU, "", landuse)
  newfile<-paste0("data/GIS_downloads/LU2013/", fn)
  tryCatch(download.file(landuse, destfile = newfile), error= function(e){print(paste0(landuse, "did not download"))})
  if(file.exists(newfile)){
    unzip(newfile, exdir = gsub(".zip", "", newfile))
  }
  if(dir.exists(gsub(".zip", "", newfile))){
    file.remove(newfile)
    print(paste0("everything worked for ", fn))
  }
})


##############################
# was in downlaod_plants but think at least some of this belongs in here and other parts in crunch_GIS to keep workflows distinct and load fewer packages and maybe less data at any given time.

# get the climate data
get_chelsa(period = "current", output_dir = "data/fromR/lfs")


#make a raster stack
bc <- raster::stack(list.files("data/fromR/lfs/current/", full.name = T))

# map(13:19, function(layr){
#   print(layr)
#   raster::raster(paste0("data/fromR/lfs/current/CHELSA_bio10_", layr, ".tif"))
# })

# add bioclimatic value for each observation
good_coords<- withstats2 %>%
  filter(decimalLatitude<44 & decimalLatitude> 34 &decimalLongitude>-82 & decimalLongitude < -73) # some errors, check workflow that they weren't introduced here.

#df of lat and long
localities <- good_coords %>%
  group_by(decimalLatitude, decimalLongitude) %>%
  summarize(n()) %>%
  select( longitude = decimalLongitude, latitude = decimalLatitude)


#crop the rasters based on extent
bounds<-raster::extent(matrix(c(min(localities$longitude), min(localities$latitude), max(localities$longitude), max(localities$latitude)), nrow = 2))

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
lulc_shape<-readOGR("data/LULC_2010/")
ll<-SpatialPoints(localities)

proj4string(lulc_shape)
proj4string(ll)<-proj4string(lulc_shape)
tictoc::tic()
lulc2010<-sp::over(ll, lulc_shape)
tictoc::toc()

climUseStat<-left_join(clim_stat %>%
                         rename(latitude = decimalLatitude
                                , longitude = decimalLongitude)
                       , bind_cols(localities, lulc2010))

# names(clim_stat)<-make.names(clim_stat)
# write data to .csv
data.table::fwrite(climUseStat[,-172], "data/fromR/lfs/plants_1989-2019_with_status_climate_landUse.csv", row.names =F)
