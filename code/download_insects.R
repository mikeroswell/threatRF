#get bug occurrences and statuses

library(natserv) #this is how we get naturserve status
library(rgbif) #how we get gbif observations
library(tidyverse)
library(patchwork)

# when downloading data from gbif, will need to combine different record types into DF
bind.gbif<-function(gbif){bind_rows(gbif[[2]][[3]], gbif[[3]][[3]])}



# See if we can get the taxonKey for Lepidoptera and then for bees

#don't know how to do this smart but using the webtool drop down I found the taxon keys for the bee families, then make a semicolon-separated string to search.
halic<-7908
apida<-4334
andre<-7901
megac<-7911
colle<-7905
melli<-4345

keyall<-paste(halic, apida, andre, megac, colle, melli, sep=";")

MD_hym <- occ_search(taxonKey = keyall
                     , stateProvince = "Maryland", year="1989, 2019"
                     , basisOfRecord = c("OBSERVATION", "HUMAN_OBSERVATION", "PRESERVED_SPECIMEN")
                     , hasCoordinate = T
                     , limit = 1e5)

hym_flat<-bind.gbif(MD_hym)

MD_lep<- occ_search(taxonKey = 797
                    , stateProvince = "Maryland", year="1989, 2019"
                    , basisOfRecord = c("OBSERVATION", "HUMAN_OBSERVATION", "PRESERVED_SPECIMEN")
                    , hasCoordinate = T
                    , limit = 1e5)
lep_flat<-bind.gbif(MD_lep)
