#get bug occurrences and statuses
# update natserv from time to tim
# remotes::install_github("ropensci/natserv")
library(natserv) #this is how we get naturserve status
library(rgbif) #how we get gbif observations
library(tidyverse)
library(patchwork)

# when downloading data from gbif, will need to combine different record types into DF
bind.gbif<-function(gbif){bind_rows(gbif[[2]][[3]], gbif[[3]][[3]])}



# See if we can get the taxonKey for Lepidoptera and then for bees

#don't know how to do this smart but using the webtool drop down I found the taxon keys for the bee families, then make a semicolon-separated string to search.
# halic<-7908
# apida<-4334
# andre<-7901
# megac<-7911
# colle<-7905
# melli<-4345
# 
# keyall<-paste(halic, apida, andre, megac, colle, melli, sep=";")
# 
# MD_hym <- occ_search(taxonKey = keyall
#                      , stateProvince = "Maryland", year="1989, 2019"
#                      , basisOfRecord = c("OBSERVATION", "HUMAN_OBSERVATION", "PRESERVED_SPECIMEN")
#                      , hasCoordinate = T
#                      , limit = 1e5)
# 
# hym_flat<-bind.gbif(MD_hym)

MD_lep<- occ_search(taxonKey = 797
                    , stateProvince = "Maryland", year="1989, 2021"
                    , basisOfRecord = c("OBSERVATION", "HUMAN_OBSERVATION", "PRESERVED_SPECIMEN")
                    , hasCoordinate = T
                    , limit = 1e5)
lep_flat<-bind.gbif(MD_lep)

leps_gs<-lep_flat %>% separate(acceptedScientificName, sep =" "
                      , into =c("genus", "species")) %>% # separate just drops stuff after the first two!
  mutate(gs = paste(genus, species, sep = "_")
         , withspace = paste(genus, species, sep = " ")) #convenient to keep track of binomials in several forms?

# write gbif data to file (make these steps modular since they take a long time)
write.csv(leps_gs, "data/fromR/lfs/leps_direct_from_gbif.csv", row.names = F)

lep_flat
leps_of_MD<-unique(leps_gs$withspace) 
length(leps_of_MD) #1749
###################################################

#next, download status classifications from natureserve
lep_stats <- ns_search_spp(species_taxonomy = list(scientificTaxonomy = "Lepidoptera", level = "order")
                             , location = list(nation ="US", subnation ="MD") #this filters to only include species that have a MD status, but retains status for all localities
                             , page = 0
                             , per_page = 5e3)[[1]] %>%
  unnest(cols=nations) %>%
  unnest (cols = "subnations", names_repair ="unique") %>%
  filter(subnationCode == "MD") #here is the step where I drop other localities, but this could be dropped at some point.

#write just the natureserve data to file
write.csv(plant_stats[,-21], "data/fromR/lfs/plant_NS_data.csv", row.names = F)