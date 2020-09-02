### This file runs the RF approach for the bees including those from GBIF
library(randomForest)
# library(plyr)
library(dplyr)
# memory.limit(size=20000)

# setwd("E:/UMD/Projects/BeesOfMD/GBIF/0034093-190918142434337/")

##load files with taxonomy and with spatial data
# ObservCons = read.csv(file="ObsMDBeesSamGBIFConservStat.csv") #read observations with conservation categories
ObservCons <- fread("data/fromR/ObsMDBeesSamGBIFConservStatMR.csv")
# ObservCons=ObservCons[,c(2,15:32)]
# ObservCons=ObservCons[,c(1:6,18,19)]

#ObsConsSpatRed= ObservCons %>% distinct(species, .keep_all = TRUE) #this cleans up all the extra lines for the same species
#ObservCons=ObservConsRed


SpatialVariables = read.delim(file="data/ClimateSummariesBees.txt", sep="\t", header=T) # read summaries of spatial variables per bee species

#clean names here too!
SpatialVariables = SpatialVariables %>%  mutate(gs = str_to_sentence(SpatialVariables$species.i)) %>% # deal with capitalization errors
  separate(col = "gs", into = c("genus", "species"), sep = " ") %>% cleannames()

SpatialVariables %>% anti_join(traits, by ="gs")
##connect the two files
ObsConsSpat=merge.default(ObservCons,SpatialVariables,by.x="gs",by.y="gs",all=T) #this takes a few seconds.
ObsConsSpatRed= ObsConsSpat %>% distinct(gs, .keep_all = TRUE) #this cleans up all the extra lines for the same species #ok but why did I want that??
# ObsConsSpat=ObsConsSpatRed

#get some summaries of the data
summary(ObsConsSpatRed[ObsConsSpatRed$StateCatComb=="S1",])


boxplot(area~StateCatComb,data=ObsConsSpatRed)
boxplot(n.gps~StateCatComb,data=ObsConsSpatRed)




####################################################################################
############################ PREPARE DATA FOR RF ###################################
####################################################################################



#remove those with low sample size: changing threshhold to 2!
ObsConsSpat_2<-subset(ObsConsSpatRed, n.gps > 1) #retains ~50 spp excluded from >3!

#get numbers for table
table(ObsConsSpat_2$StateCatComb)
sum(is.na(ObsConsSpat_2$StateCatComb))

#create a new column with the new reclassified categories (move SU, SNR to NA)
ObsConsSpat_2$SumStateCat<-ObsConsSpat_2$StateCatComb
levels(ObsConsSpat_2$SumStateCat) <- c(levels(ObsConsSpat_2$SumStateCat), "S1S2S3","S4S5") #add new levels for that column; otherwise it will print NA's

ObsConsSpat_2$SumStateCat[ObsConsSpat_2$SumStateCat == "SU"] <-"NA"
ObsConsSpat_2$SumStateCat[ObsConsSpat_2$SumStateCat == "SNR"] <-"NA"
ObsConsSpat_2$SumStateCat[ObsConsSpat_2$SumStateCat == "S1"] <-"S1S2S3"
ObsConsSpat_2$SumStateCat[ObsConsSpat_2$SumStateCat == "S2S3"] <-"S1S2S3"
ObsConsSpat_2$SumStateCat[ObsConsSpat_2$SumStateCat == "S3"] <-"S1S2S3"
ObsConsSpat_2$SumStateCat[ObsConsSpat_2$SumStateCat == "S4"] <-"S4S5"
ObsConsSpat_2$SumStateCat[ObsConsSpat_2$SumStateCat == "S5"] <-"S4S5"
ObsConsSpat_2$SumStateCat[ObsConsSpat_2$SumStateCat == "SH"] <-"S1S2S3"
ObsConsSpat_2$SumStateCat[ObsConsSpat_2$SumStateCat == "S1S3"] <-"S1S2S3"
ObsConsSpat_2 = droplevels(ObsConsSpat_2)
table(ObsConsSpat_2$SumStateCat)

#### 26 spp not NA or unclassified. 2 are in threatened category? Uh OH!
ObsConsSpat_2 %>% filter(SumStateCat =="S1S2S3") %>% select(gs)
####################################################################################
############################ RUN RF ###################################
####################################################################################
# setwd("E:/UMD/Projects/BeesOfMD/GBIF/0034093-190918142434337/RF/Full")


NABees=ObsConsSpat_2[is.na(ObsConsSpat_2$SumStateCat),] # keep all the NAs
ForAnalysisBees = ObsConsSpat_2[!is.na(ObsConsSpat_2$SumStateCat),] #remove all the NAs
ForAnalysisBees = ForAnalysisBees[!is.na(ForAnalysisBees$area),] #remove all NAs from area
ForAnalysisBees = droplevels(ForAnalysisBees) # drop all levels that are not used anymore

fitRF <- randomForest(SumStateCat ~ area
                      +max_lat
                      +min_lat
                      +abs_max_lat
                      +abs_min_lat
                      +length_lat
                      +median_lon
                      +median_lat
                      +CHELSA_bio10_1
                      +CHELSA_bio10_2
                      +CHELSA_bio10_3
                      +CHELSA_bio10_4
                      +CHELSA_bio10_5
                      +CHELSA_bio10_6
                      +CHELSA_bio10_7
                      +CHELSA_bio10_8
                      +CHELSA_bio10_9
                      +CHELSA_bio10_10
                      +CHELSA_bio10_11
                      +CHELSA_bio10_12
                      +CHELSA_bio10_13
                      +CHELSA_bio10_14
                      +CHELSA_bio10_15
                      +CHELSA_bio10_16
                      +CHELSA_bio10_17
                      +CHELSA_bio10_18
                      +CHELSA_bio10_19
                      +CHELSA_bio10_1SD
                      +CHELSA_bio10_2SD
                      +CHELSA_bio10_3SD
                      +CHELSA_bio10_4SD
                      +CHELSA_bio10_5SD
                      +CHELSA_bio10_6SD
                      +CHELSA_bio10_7SD
                      +CHELSA_bio10_8SD
                      +CHELSA_bio10_9SD
                      +CHELSA_bio10_10SD
                      +CHELSA_bio10_11SD
                      +CHELSA_bio10_12SD
                      +CHELSA_bio10_13SD
                      +CHELSA_bio10_14SD
                      +CHELSA_bio10_15SD
                      +CHELSA_bio10_16SD
                      +CHELSA_bio10_17SD
                      +CHELSA_bio10_18SD
                      +CHELSA_bio10_19SD
                    , data=ForAnalysisBees
                    , importance=TRUE
                    , ntree=1000
                    , replace=T
                    , votes=T
                    , na.action = na.exclude)

fitRF
Contribution=fitRFPNW$importance
write.table(Contribution,file="GBIFFullVariableContrib.csv", sep=",", append=T)


errorRates=fitRFPNW$err.rate
write.table(errorRates, file="GBIFFullError.csv", sep=",", append=T)

predictRF<-predict(fitRF, newdata=NABees, type="prob")
df<-data.frame(NABees$name, predictRF)
write.csv(df, file="BeesMDGBIFPredictAllDataFull.csv", row.names=FALSE)

#########################Downsample to match min class (2)
setwd("E:/UMD/Projects/BeesOfMD/GBIF/0034093-190918142434337/RF/Downsample")

for (i in 1:1000) {
  PNWRF=c()
  fitRFPNW=c()
  binded.Predict=c()
  PNWRF=ForAnalysisBees
  C=PNWRF[PNWRF$SumStateCat=="S1S2S3",]
  Csel=C[(sample(nrow(C), size=7, replace=T)),]
  NC=PNWRF[PNWRF$SumStateCat=="S4S5",]
  PNWRF=rbind(Csel,NC)

  fitRFPNW <- randomForest(SumStateCat ~ area+max_lat+min_lat+abs_max_lat+abs_min_lat+length_lat+median_lon+median_lat+CHELSA_bio10_1+CHELSA_bio10_2+CHELSA_bio10_3+
                             CHELSA_bio10_4+CHELSA_bio10_5+CHELSA_bio10_6+CHELSA_bio10_7+CHELSA_bio10_8+CHELSA_bio10_9+CHELSA_bio10_10+CHELSA_bio10_11+CHELSA_bio10_12+
                             CHELSA_bio10_13+CHELSA_bio10_14+CHELSA_bio10_15+CHELSA_bio10_16+CHELSA_bio10_17+CHELSA_bio10_18+CHELSA_bio10_19+CHELSA_bio10_1SD+
                             CHELSA_bio10_2SD+CHELSA_bio10_3SD+CHELSA_bio10_4SD+CHELSA_bio10_5SD+CHELSA_bio10_6SD+CHELSA_bio10_7SD+CHELSA_bio10_8SD+CHELSA_bio10_9SD+
                             CHELSA_bio10_10SD+CHELSA_bio10_11SD+CHELSA_bio10_12SD+CHELSA_bio10_13SD+CHELSA_bio10_14SD+CHELSA_bio10_15SD+CHELSA_bio10_16SD+
                             CHELSA_bio10_17SD+CHELSA_bio10_18SD+CHELSA_bio10_19SD, data=PNWRF, importance=TRUE, ntree=1000, replace=T, votes=T)

  Contribution=fitRFPNW$importance
  write.table(Contribution,file="Downsample11BeesMDGBIF.csv", sep=",", append=T)

  SpeciesPredicted = fitRFPNW$votes
  SpeciesInfo =  PNWRF[row.names(fitRFPNW$votes),c(1,56)]
  SpeciesPredicted = cbind(SpeciesPredicted, SpeciesInfo)
  write.table(SpeciesPredicted, file="PredictionOfOOBBeesMDGBIF.csv", sep=",", append=T)


  errorRates=fitRFPNW$err.rate
  write.table(errorRates, file="Downsample11BeesMDErrorGBIF.csv", sep=",", append=T)

  PredictionPNW <- predict(fitRFPNW)
  binded.dataRFPNW=c()
  binded.dataRFPNW=cbind(PNWRF, PredictionPNW) # bind initial data table with predicted values
  write.table(binded.dataRFPNW,file="binded.dataDownsample11BeesMD.csv", sep=",", append=T)

  ctRFPNW <- table(binded.dataRFPNW$SumStateCat, PredictionPNW)
  diag(prop.table(ctRFPNW, 1))
  write.table(ctRFPNW, file="confusionTableDownsample11BeesMDGBIF.csv", sep=",",append=T)

  NABees=ObsConsSpat[is.na(ObsConsSpat$SumStateCat),] # keep all the NAs
  PredictionNEWprob <- predict(fitRFPNW,NABees,type="prob") #calculates probability for each group
  PredictionNEW <- predict(fitRFPNW,NABees) # predict by assigning to category only
  binded.dataRFPNW=cbind(NABees, PredictionNEWprob,PredictionNEW)
  binded.Predict=rbind(binded.dataRFPNW,binded.Predict)
  write.table(binded.Predict, file="PredictNAData_Downsample11BeesMDGBIF.csv", sep=',', append=T)

}


#################EXTRACT SPECIES AND SEE WHO'S PREDICTED ##########################
GBIFSamError=read.delim(file="Downsample11BeesMDErrorGBIF.csv", header=T, row.names = NULL, sep=',') # Error rates overall
GBIFSamPredictionNA=read.delim(file="PredictNAData_Downsample11BeesMDGBIF.csv", header=T, row.names = NULL, sep=',') # Read predictions of NAs
GBIFSamContrib=read.delim(file="Downsample11BeesMDGBIF.csv", header=T, row.names = NULL, sep=',')

boxplot(S1S2S3~species, data=GBIFSamPredictionNA[!is.na(GBIFSamPredictionNA$S1S2S3>0.8),], las=2) #plot species that are predicted at high probability
=aggregate( S1S2S3 ~ species, GBIFSamPredictionNA, mean )
write.csv(SpeciesPrediction, file="GBIFSamDownsamplingPerSpecies.csv")

MDA=aggregate( as.numeric(MeanDecreaseAccuracy) ~ row.names, GBIFSamContrib, mean )
Gini=aggregate( as.numeric(MeanDecreaseGini) ~ row.names, GBIFSamContrib, mean )
GiniSorted=Gini[order(Gini$`as.numeric(MeanDecreaseGini)`),]



PredictAllData=read.csv(file="BeesMDPredictAllData.csv", header=T) #read results from RF
SpeciesNoLC = PredictAllData[PredictAllData$S1S2S3>=0.5,] # select species that were predicted as protection category at >0.5
SpeciesNoLC=SpeciesNoLC[!is.na(SpeciesNoLC$NABees.name),] ## remove lines with NAs
SpeciesNoLC<- droplevels(SpeciesNoLC) #drop unused levels (names of species that are not in the selection)
SpeciesNoLC.names=levels(SpeciesNoLC$NABees.name) # pull names of species

SpeciesLC = PredictAllData[PredictAllData$S1S2S3<0.5,] #select species that were predicted as protection category <0.5
SpeciesLC<- droplevels(SpeciesLC) #drop unused levels (names of species that are not in the selection)
SpeciesLC.names=levels(SpeciesLC$NABees.name) # pull names of species


PredictBeesTraits=ObsConsSpat[ObsConsSpat$name==SpeciesLC.names,]
