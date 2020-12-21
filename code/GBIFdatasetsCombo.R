library(maps)
library(rgdal)
library(rgeos)
library(dismo)
library(tidyverse)
library(data.table)

#####This prepares the gbif data####


# setwd("E:/UMD/Projects/BeesOfMD/GBIF/0034093-190918142434337")

GBIFData=read.delim(file="data/0034093-190918142434337.csv", header=T, sep="\t") #read data from GBIF
GBIFDataCoord=GBIFData[!is.na(GBIFData$decimalLatitude),] # delete observations that have no geo coord
GBIFDataCoord=GBIFData[!is.na(GBIFData$decimalLongitude),] # delete observations that have no geo coord
coordinates(GBIFDataCoord) <- c("decimalLongitude", "decimalLatitude")


marylandST <- readOGR(dsn = "data/stateBoundaries/Maryland_Political_Boundaries__State_Boundary",
                      layer = "Maryland_Political_Boundaries__State_Boundary")

marylandCt <- readOGR(dsn = "data/stateBoundaries/Maryland_Physical_Boundaries__County_Boundaries_Generalized",
                      layer = "Maryland_Physical_Boundaries__County_Boundaries_Generalized")
proj4string(GBIFDataCoord) <- marylandST@proj4string


GBIFMD <- over(GBIFDataCoord, marylandST) #identifies points that fall within MD
GBIFDataCoordDF = data.frame(GBIFDataCoord)
GBIFMD <- subset(GBIFDataCoordDF,GBIFMD[,'OBJECTID']!="<NA>") #only selects the points that are in MD
coordinates(GBIFMD) <- c("decimalLongitude", "decimalLatitude") # creates a spatial DF for those from MD only
GBIFMD.df=data.frame(GBIFMD)
GBIFMD.df=GBIFMD.df[!GBIFMD.df$collectionCode=="USGS PWRC - Native Bee Inventory and Monitoring Lab (BIML)",] #deletes observations from the bee lab

families=c("Andrenidae","Apidae","Halictidae","Melittidae","Megachilidae","Colletidae")
GBIFBeesOnly=c()
#this will select only the Bee families from GBIF
for (i in 1:length(families)){
GBIFfam=GBIFMD.df[GBIFMD.df$family==families[i],]
GBIFBeesOnly=rbind(GBIFBeesOnly,GBIFfam)
}
GBIFBeesOnly=droplevels(GBIFBeesOnly)
GBIFBeesOnly <- cleannames(GBIFBeesOnly %>% dplyr::rename(gs = species) %>% dplyr::select(-genus))

names(GBIFBeesOnly)

GBIFBeesOnly %>% anti_join(traits, by = c("genus", "species")) %>% group_by(gs) %>% dplyr::summarize(n())

plot(marylandST)
points(GBIFMD, pch=20, col=GBIFMD$genus) #plots the points in MD and colors them by species


##########This combines GBIF and Sam ###########

SamBees <- fread("data/fromR/Obs_stat_cleaned.csv")
# SamBees=read.csv(file="E:/UMD/Projects/BeesOfMD/USGS_DRO_flat/ObsMDBeesConservStat.csv", header=T)

SWAPCat <- read.csv("data/SWAP_hymMD.csv")

# SWAPCat=read.csv(file="E:/UMD/Projects/BeesOfMD/USGS_DRO_flat/SWAP_hymMD.csv", sep=",", header=T) # read conservation category


# NatureServe=read.table(file="E:/UMD/Projects/BeesOfMD/USGS_DRO_flat/NatureServeData.txt", sep="\t", header=T)

NatureServe=read.table(file="data/NatureServeData.txt", sep="\t", header=T)
# tic()
# AllBees = merge(GBIFBeesOnly,SamBees,by.x="gs", by.y="gs", all=T) #merges the two datasets # combines the two datasets #this is really really slow!!!
# toc() #this took almost 2 min
# tic()
AllBees<-SamBees %>% left_join(GBIFBeesOnly, by = "gs")
# toc() # under 20 seconds!
### create one file with all conservation categories (nature Serve and SWAP)
ConsCategory = merge.default(SWAPCat,NatureServe, by.x='Scientific.name', by.y="ScientificName", all=T)
ConsCategory$GlobalCatComb <- ConsCategory$GRANK #create a new column to combine both SWAP and NatServe for global category
levels(ConsCategory$GlobalCatComb) <- c(levels(ConsCategory$GlobalCatComb), "G5","G5?","G4","G4?","G3","G2G3","G3G5") #add new levels for that column; otherwise it will print NA's
ConsCategory$GlobalCatComb[is.na(ConsCategory$GlobalCatComb)] <- (ConsCategory$NatureServeGS[is.na(ConsCategory$GlobalCatComb)]) #replace values from one on the other

ConsCategory$StateCatComb <- ConsCategory$SRANK #create a new column to combine both SWAP and NatServe for state category
levels(ConsCategory$StateCatComb) <- c(levels(ConsCategory$StateCatComb), "S5","S4") #add new levels for that column; otherwise it will print NA's
ConsCategory$StateCatComb[is.na(ConsCategory$StateCatComb)] <- (ConsCategory$Maryland[is.na(ConsCategory$StateCatComb)]) #replace values from one on the other
ConsCategory$StateCatComb[ConsCategory$GlobalCatComb=="G4"] <- "S4"
ConsCategory$StateCatComb[ConsCategory$GlobalCatComb=="G4?"] <- "S4"
ConsCategory$StateCatComb[ConsCategory$GlobalCatComb=="G5"] <- "S5"
ConsCategory$StateCatComb[ConsCategory$GlobalCatComb=="G5?"] <- "S5"

#combine list of occurrences with their categories
# ObsMDConsCat = merge.default(AllBees[,c(1,2,4,8:14,22:23,64:66,89:91)],ConsCategory, by.x='species', by.y="Scientific.name", all=T)

ObsMDConsCat = left_join(AllBees, ConsCategory, by = c("gs" = "Scientific.name"))

ObsMDConsCat <- ObsMDConsCat %>% select(-(grep(pattern = "*.x", names(ObsMDConsCat))))
names(ObsMDConsCat)<- gsub(pattern = "*.y", replacement = "", names(ObsMDConsCat))


# have all coodinates appear under the same column
ObsMDConsCat$latitude[is.na(ObsMDConsCat$latitude)] <- (ObsMDConsCat$decimalLatitude[is.na(ObsMDConsCat$latitude)]) #replace values from one on the other
ObsMDConsCat$longitude[is.na(ObsMDConsCat$longitude)] <- (ObsMDConsCat$decimalLongitude[is.na(ObsMDConsCat$longitude)]) #replace values from one on the other

fwrite(ObsMDConsCat, file="data/fromR/ObsMDBeesSamGBIFConservStatMR.csv") #write table with all this organized data



