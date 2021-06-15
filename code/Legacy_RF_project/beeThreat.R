### This file runs the RF approach for the bees
library(maps)
library(rgdal)
library(rgeos)
library(dismo)
library(taxize)
library(natserv)
library(plyr)
library(raster)
library(matrixStats)

setwd("E:/UMD/Projects/BeesOfMD/Bioclim")


#### read climatic files (from CHELSA) and then produce a raster stack of all of them
bio1w = raster("CHELSA_bio10_1.tif")
bio2w = raster("CHELSA_bio10_2.tif")
bio3w = raster("CHELSA_bio10_3.tif")
bio4w = raster("CHELSA_bio10_4.tif")
bio5w = raster("CHELSA_bio10_5.tif")
bio6w = raster("CHELSA_bio10_6.tif")
bio7w = raster("CHELSA_bio10_7.tif")
bio8w = raster("CHELSA_bio10_8.tif")
bio9w = raster("CHELSA_bio10_9.tif")
bio10w = raster("CHELSA_bio10_10.tif")
bio11w = raster("CHELSA_bio10_11.tif")
bio12w = raster("CHELSA_bio10_12.tif")
bio13w = raster("CHELSA_bio10_13.tif")
bio14w = raster("CHELSA_bio10_14.tif")
bio15w = raster("CHELSA_bio10_15.tif")
bio16w = raster("CHELSA_bio10_16.tif")
bio17w = raster("CHELSA_bio10_17.tif")
bio18w = raster("CHELSA_bio10_18.tif")
bio19w = raster("CHELSA_bio10_19.tif")

ClimateStack = stack(bio1w,bio2w,bio3w,bio4w,bio5w,bio6w,bio7w,bio8w,bio9w,bio10w,bio11w,bio12w,bio13w,bio14w,bio15w,bio16w,bio17w,bio18w,bio19w)

#read file with all points and info

setwd("E:/UMD/Projects/BeesOfMD/GBIF/0034093-190918142434337")

d = read.csv(file="ObsMDBeesSamGBIFConservStat.csv")
d = d[,c(2,3,5,6,7,10,14,15:19,31:32)]
d= d[!is.na(d$latitude),]
dSpatial = d
coordinates(dSpatial) <- c("longitude", "latitude") # make those points spatial points


#extract values for each point
ClimateDataObs = extract(ClimateStack,dSpatial) #extract bioclim variables for each point
ClimateBees=cbind(dSpatial,ClimateDataObs) # connects the observations table with climatic extractions

#calculate climatic mean + SD per species
ClimateBees@data$species = droplevels(ClimateBees@data$species)
species = as.vector(levels(ClimateBees$species))
variables = colnames(ClimateDataObs)


for (i in 1:length(species)){
  print(species[i])
  df<-ClimateBees[which(ClimateBees@data$species==species[i]),] #select each species
  n.gps<-nrow(df) #pull out observations line number
  xy<-as.data.frame(df@coords) #pull out coords for those obs

  #calculate summaries
  max_lat <- max(xy[,2])
  min_lat <- min(xy[,2])
  abs_max_lat <- abs(max_lat)
  abs_min_lat <- abs(min_lat)
  length_lat <- max_lat - min_lat
  median_lon <- median(xy[,1])
  median_lat <- median(xy[,2])

  ch <- chull(xy) #calculate area between coordinates
  coords <- xy[ch,] #get coords of that area
  sp_poly <- SpatialPolygons(list(Polygons(list(Polygon(coords)), ID=1))) #create a polygon fro that area

  if (n.gps > 2){
#    sp_poly_crop<-gIntersection(sp_poly,byid=T,drop_lower_td=TRUE)
    area<-gArea(sp_poly) #calculate area for polygon
    format(area, scientific = FALSE)
  } else {
    area <- NA
  }

  ClimateMean<-as.data.frame(colMeans(df@data[,13:31], na.rm=TRUE)) #calculate mean + SD for each of the climatic variables
  ClimateSD<-as.data.frame((colSds(as.matrix(df@data[,13:31]), na.rm=TRUE)))
  rownames(ClimateSD)<-c(paste(rownames(ClimateMean),"SD",sep=""))
  ClimateMean=t(ClimateMean)
  ClimateSD=t(ClimateSD)

  #write it all out by combining it
  write.table(data.frame(species[i], n.gps, ClimateMean, ClimateSD, area, max_lat, min_lat,abs_max_lat,
                         abs_min_lat,length_lat,median_lon,median_lat), file="ClimateSummariesBeesGBIF.txt", quote=FALSE,
                         row.names=FALSE, col.names=!file.exists("ClimateSummariesBeesGBIF.txt"), append=TRUE, sep="\t")
}



