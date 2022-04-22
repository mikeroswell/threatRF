devtools::install_github(https://github.com/mikeroswell/natserv-1.git) # this has a little fix for searching by family, genus, etc.
library(natserv) #this is how we get naturserve status
library(rgbif) #how we get gbif observations
library(tidyverse)
library(patchwork)

# when downloading data from gbif, will need to combine different record types into DF
bind.gbif<-function(gbif){bind_rows(gbif[[2]][[3]], gbif[[3]][[3]])}

uscore_binomial<-function(x){gsub("+ + *", "+_+",x)}

`%ni%` <- Negate(`%in%`) #convenience, this should be part of base R!



halic<-7908
apida<-4334
andre<-7901
megac<-7911
colle<-7905
melli<-4345

keyall<-paste(halic, apida, andre, megac, colle, melli, sep=";")

MN_hym <- occ_search(taxonKey = keyall
                     , stateProvince = "Minnesota", year="1989, 2021"
                     , basisOfRecord = c("OBSERVATION", "HUMAN_OBSERVATION", "PRESERVED_SPECIMEN")
                     , hasCoordinate = T
                     , limit = 1e5)

MN_Bee_records<-MN_hym %>% 
  bind.gbif() %>% 
  separate(acceptedScientificName, sep =" "
         , into =c("genus", "species")) %>% # separate just drops stuff after the first two!
  mutate(gs = paste(genus, species, sep = "_")
         , withspace = paste(genus, species, sep = " "))


bee_status <- map_dfr(c("Halictidae"
                        , "Apidae"
                        , "Andrenidae"
                        , "Megachilidae"
                        , "Colletidae"
                        , "Melittidae"), function(fam){
                          print(fam)
  ns_search_spp(species_taxonomy = list(scientificTaxonomy = fam, level = "family", kingdom = "Animalia")
                           , location = list(nation ="US", subnation ="MN") #this filters to only include species that have a MD status, but retains status for all localities
                           , page = 0
                           , per_page = 5e3)[[1]] %>%
  unnest(cols=nations) %>%
  unnest (cols = "subnations", names_repair ="unique") %>%
  filter(subnationCode == "MN")
                        })


bee_status %>% group_by(roundedNRank) %>% summarize(n())

View(bee_status %>% filter(roundedNRank %ni% c("NNA", "NNR", "NU")))

MN_bees_with_status<-MN_Bee_records %>% left_join(bee_status, by=c("withspace"="scientificName"))

View(MN_bees_with_status %>% filter(genus != "Bombus") %>% filter(roundedNRank %ni% c("NNA", "NNR", "NU")) %>% group_by(genus, species, roundedNRank) %>% summarize(n()))
