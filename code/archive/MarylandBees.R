########## evaluate the bees of MD and their ecol stuf
library(maps)
library(rgdal)
library(rgeos)
library(dismo)
library(taxize)
library(natserv)
library(tidyverse)
options(NatureServeKey = "2fec2d9b-0af1-42b8-afd0-b0e5cbf5c23b")

# setwd("E:/UMD/Projects/BeesOfMD/USGS_DRO_flat")

########FOR THE FIRST TIME
###first, read the files
obs= read.csv("data/USGS_DRO_flat.csv", header=T) #read observations
# obs[,13:14] <- as.numeric(as.matrix(unlist(obs[,13:14]))) #transform coodinate columns to numeric

obs<-cleannames(obs, "name")


SpeciesOnly <- as.matrix(unique(obs$gs)) #gets all species names to search in the following lines
TaxObsFull=c()
TaxObs <- tax_name(SpeciesOnly[1:483],get = c("genus","family","order"), db="itis", rows=1) #searches taxonomy and picks hte first line if asked to choose.
TaxObsFull = rbind(TaxObsFull,TaxObs)# get taxonomy
TaxObs <- tax_name(SpeciesOnly[c(483,485:2577)],get = c("genus","family","order"), db="itis", rows=1) #searches taxonomy and picks hte first line if asked to choose.
TaxObsFull = rbind(TaxObsFull,TaxObs) # get taxonomy
TaxObs <- tax_name(SpeciesOnly[c(2579:3207)],get = c("genus","family","order"), db="itis", rows=1) #searches taxonomy and picks hte first line if asked to choose.
TaxObsFull = rbind(TaxObsFull,TaxObs) # get taxonomy
TaxObs <- tax_name(SpeciesOnly[c(3209:3600)],get = c("genus","family","order"), db="itis", rows=1) #searches taxonomy and picks hte first line if asked to choose.
TaxObsFull = rbind(TaxObsFull,TaxObs) # get taxonomy

SpTax=merge(obs,TaxObsFull,by.x="gs",by.y="query",all=T) #link taxonomy with actual original table
write.table(SpTax, file="AllTaxaWithTaxonomy.csv", sep=',') #write a file wiht all the data together

SpTax=read.csv("data/AllTaxaWithTaxonomy.csv")

SpTaxnoNA <- SpTax[!is.na(SpTax$latitude),] #get rid of observations with no latitude
SpTaxnoNA <- SpTaxnoNA[!is.na(SpTaxnoNA$longitude),] # get rid of observations with no longitude
coordinates(SpTaxnoNA) <- c("longitude", "latitude") # make those points spatial points


#select points that fall within MD

obs_MD <- over(SpTaxnoNA, marylandST) #identifies points that fall within MD
SpTaxnoNADF = data.frame(SpTaxnoNA)
obsMDOnly <- subset(SpTaxnoNADF,obs_MD[,'OBJECTID']!="<NA>") #only selects the points that are in MD
coordinates(obsMDOnly) <- c("longitude","latitude") # creates a spatial DF for those from MD only
write.table(obsMDOnly, file="data/MDObsWithTaxa.csv", sep=',')

################################################################################
##########AFTER ALL TEH FILES HAVE BEEN TREATED, WE CAN JUST LOAD THE FULL FILES
#################################################################################


SpTaxnoNA <- read.csv(file="data/AllTaxaWithTaxonomy.csv", header=T) #reads all the data points
SpTaxnoNA <- SpTaxnoNA[!is.na(SpTaxnoNA$latitude),] #get rid of observations with no latitude
SpTaxnoNA <- SpTaxnoNA[!is.na(SpTaxnoNA$longitude),] # get rid of observations with no longitude
coordinates(SpTaxnoNA) <- c("longitude", "latitude") # make those points spatial points

obsMDOnly <- read.csv(file="data/MDObsWithTaxa.csv", header=T) #reads all the MD data points
coordinates(obsMDOnly) <- c("longitude", "latitude") # make those points spatial points

###read all map files

marylandST <- readOGR(dsn = "./stateBoundaries/Maryland_Political_Boundaries__State_Boundary",
                      layer = "Maryland_Political_Boundaries__State_Boundary")

marylandCt <- readOGR(dsn = "./stateBoundaries/Maryland_Physical_Boundaries__County_Boundaries_Generalized",
                      layer = "Maryland_Physical_Boundaries__County_Boundaries_Generalized")
proj4string(SpTaxnoNA) <- marylandST@proj4string

#load NatureServe dataset with state categories
NatureServe=read.table(file="NatureServeData.txt", sep="\t", header=T)

###read tables with other information we have
SWAPCat=read.csv(file="SWAP_hymMD.csv", sep=",", header=T) # read conservation category
HostPlant=read.table(file="BeeHostPlants.txt", sep="\t", header=T) # read plant specialization

FullDataHost = merge(obsMDOnly,HostPlant,by.x="name",by.y="Science.name",all=T) #merge observations with plant specialization
FullDataHost = merge(FullDataHost,SWAPCat,by.x="name",by.y="Scientific.name",all=T) #merge observations with plant specialization
FullDataHost = merge(FullDataHost,NatureServe,by.x="name",by.y="ScientificName",all=T) #merge observations with plant specialization


#make a map and see how many points are in MD
plot(marylandST)
points(obsMDOnly, pch=20, col=obsMDOnly$family) #plots the points in MD and colors them by species

plot(marylandST)
points(FullDataHost, pch=20, col=FullDataHost$Maryland, na.action(na.omit)) #plots the points in MD and colors them by species

### create one file with all categories conservation categories (nature Serve and SWAP)
ConsCategory = merge.default(SWAPCat,NatureServe, by.x='Scientific.name', by.y="ScientificName", all=T)
ConsCategory$GlobalCatComb <- ConsCategory$GRANK #create a new column to combine both SWAP and NatServe for global category
levels(ConsCategory$GlobalCatComb) <- c(levels(ConsCategory$GlobalCatComb), "G5","G5?","G4","G4?","G3","G2G3","G3G5") #add new levels for that column; otherwise it will print NA's
ConsCategory$GlobalCatComb[is.na(ConsCategory$GlobalCatComb)] <- (ConsCategory$NatureServeGS[is.na(ConsCategory$GlobalCatComb)]) #replace values from one on the other

levels(ConsCategory$StateCatComb) <- c(levels(ConsCategory$StateCatComb), "S5","S4") #add new levels for that column; otherwise it will print NA's
ConsCategory$StateCatComb <- ConsCategory$SRANK #create a new column to combine both SWAP and NatServe for state category
ConsCategory$StateCatComb[is.na(ConsCategory$StateCatComb)] <- (ConsCategory$Maryland[is.na(ConsCategory$StateCatComb)]) #replace values from one on the other
ConsCategory$StateCatComb[ConsCategory$GlobalCatComb=="G4"] <- "S4"
ConsCategory$StateCatComb[ConsCategory$GlobalCatComb=="G4?"] <- "S4"
ConsCategory$StateCatComb[ConsCategory$GlobalCatComb=="G5"] <- "S5"
ConsCategory$StateCatComb[ConsCategory$GlobalCatComb=="G5?"] <- "S5"


###combine observations of bee species with their conservation category
ObsMDConsCat = merge.default(obsMDOnly,ConsCategory, by.x='name', by.y="Scientific.name", all=T)

### Filter only Hymenoptera and delete wasps and such
ObsMDBees = ObsMDConsCat[ObsMDConsCat$order=="Hymenoptera",] # select all Hymenoptera
ObsMDBees= ObsMDBees[ObsMDBees$family!="Vespidae",]
ObsMDBees= ObsMDBees[ObsMDBees$family!="Sphecidae",]
ObsMDBees= ObsMDBees[ObsMDBees$family!="Ampulicidae",]
ObsMDBees= ObsMDBees[ObsMDBees$family!="Chrysididae",]
ObsMDBees= ObsMDBees[ObsMDBees$family!="Crabronidae",]
ObsMDBees= ObsMDBees[ObsMDBees$family!="Scoliidae",] # leave all the things that are bees
ObsMDBees = ObsMDBees[!is.na(ObsMDBees$name),] #get rid of lines with NA in species name
ObsMDBees = droplevels(ObsMDBees)



write.csv(ObsMDBees, file="ObsMDBeesConservStat.csv") #write table with all this organized data

