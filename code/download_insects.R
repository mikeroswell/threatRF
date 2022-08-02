#get bug occurrences and statuses
# update natserv from time to tim
install.packages("/System/Volumes/Data/Rstudio_Git/natserv_1.0.0.92.tar.gz", repos = NULL)
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

MD_lep_old<- occ_search(taxonKey = 797
                    , stateProvince = "Maryland", year="1989, 2005"
                    , basisOfRecord = c("OBSERVATION", "HUMAN_OBSERVATION", "PRESERVED_SPECIMEN")
                    , hasCoordinate = T
                    , limit = 1e5)
lep_flat_old<-bind.gbif(MD_lep_old)

MD_lep_new<- occ_search(taxonKey = 797
                        , stateProvince = "Maryland", year="2006, 2021"
                        , basisOfRecord = c("OBSERVATION", "HUMAN_OBSERVATION", "PRESERVED_SPECIMEN")
                        , hasCoordinate = T
                        , limit = 1e5)
lep_flat_new<-bind.gbif(MD_lep_new)

lep_flat <-bind_rows(lep_flat_new, lep_flat_old)

leps_gs<-lep_flat %>% separate(acceptedScientificName, sep =" "
                      , into =c("genus", "species")) %>% # separate just drops stuff after the first two!
  mutate(gs = paste(genus, species, sep = "_")
         , withspace = paste(genus, species, sep = " ")) #convenient to keep track of binomials in several forms?
leps_gs<-leps_gs %>% filter(!grepl("\\.", .$species))
# write gbif data to file (make these steps modular since they take a long time)
write.csv(leps_gs, "data/fromR/lfs/leps_direct_from_gbif.csv", row.names = F)

# lep_flat
# check number of spp in this dataset (or at least species names, should check)
leps_of_MD <- unique(leps_gs$withspace) 
length(leps_of_MD) #1786, an increase (maybe inat records getting research-graded?)
###################################################
leps_gs <- read.csv("data/fromR/lfs/leps_direct_from_gbif.csv")
#next, download status classifications from natureserve
lep_stats <-ns_search_spp(species_taxonomy = list(
  scientificTaxonomy = "Lepidoptera", level = "order",
    kingdom = "Animalia")
                             , location = list(nation ="US", subnation ="MD") #this filters to only include species that have a MD status, but retains status for all localities
                             , page = 0
                             , per_page = 5e3)[[1]] %>%
  unnest(cols=nations) %>%
  unnest (cols = "subnations", names_repair ="unique") %>%
  filter(subnationCode == "MD") #here is the step where I drop other localities, but this could be dropped at some point.

lep_stats %>% group_by(roundedSRank) %>% summarize(n())
# of those 300ish sp, pretty evenly split between (S5, S4, [S1, {S2, S3}]), SNR. 

#write just the natureserve data to file
write.csv(lep_stats[,-21], "data/fromR/lfs/lep_NS_data.csv", row.names = F)

lepstat<-read.csv("data/fromR/lfs/lep_NS_data.csv")

# summary data
lepstat %>% mutate(simple_status = factor(if_else(roundedSRank %in% c("S4","S5"), "secure"
                                                      , if_else(roundedSRank %in% c("S1", "S2", "S3", "SH"), "threatened", "NONE")))) %>%
  group_by(simple_status) %>% summarize(n())

lepocc<-read.csv("data/fromR/lfs/leps_direct_from_gbif.csv")

lep_joined<-lepocc %>% 
  left_join(lepstat, by=c("withspace"="scientificName")) %>% 
  dplyr::filter((!exotic...15 & !exotic...17) |(is.na(exotic...15) & is.na(exotic...17))) %>% 
  mutate(simple_status =ifelse(roundedSRank %in% c("S1", "S2", "S3", "SH"), "threat"
                               , ifelse(roundedSRank %in% c("S4", "S5"), "secure"
                                        , "unranked"))) 

write.csv(lep_joined, "data/fromR/lfs/native_leps_with_status.csv", row.names =F)
  
lep_joined %>% group_by(exotic...15) %>% summarize(n_distinct(withspace))
lep_joined %>% filter(exotic...15) %>% group_by(withspace) %>% summarize(n())
lep_joined %>% filter(is.na(exotic...15)) %>% group_by(withspace) %>% summarize(n())

lep_joined %>%
  group_by(simple_status) %>% 
  summarize(obs = n(), spp = n_distinct(withspace))
