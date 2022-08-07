#get plant and lepidopteran occurrences and statuses

# this script depends on code/tidy_flora.R
# source("code/tidy_flora.R)

library(natserv) #this is how we get naturserve status
library(rgbif) #how we get gbif observations
library(tidyverse)
library(patchwork) # cute pkg for assembling 


# # when downloading data from gbif with `occ_search`
# need to combine different record types into DF
# bind.gbif<-function(gbif){bind_rows(gbif[[2]][[3]], gbif[[3]][[3]])}

#deal with complex species name rules with a function to grab just the binomials
uscore_binomial<-function(x){gsub("+ + *", "+_+",x)}

`%ni%` <- Negate(`%in%`) #convenience, this should be part of base R!

###############################################
# get occurrence data from gbif

# now refactoriing to use occ_downlaod which comes with a doi
# this is the query sent to GBIF via the API to create a downloadable object
# likely takes several minutes.
# does not need to be redone each time code is run

# MD_vasc_and_lep_doi <- occ_download(
#   pred_in("taxonKey", c(7707728, 797))
#   , pred_in("basisOfRecord", c("OBSERVATION", "HUMAN_OBSERVATION", "PRESERVED_SPECIMEN"))
#   , pred_in("stateProvince", "Maryland")
#   , pred("hasCoordinate", TRUE)
#   , pred_and(pred_gte("year", 1989), pred_lte("year", 2021))
#   )

# # this reveals status of all gbif download requests
# occ_download_list()

# # just a test 
# occ_download(pred("gbifID", 1456777290))
 
# # this actually downloads the data (~135 MB)
# occ_download_get(MD_vasc_and_lep_doi, path = "data/fromR/lfs", overwrite = TRUE)

# another function to read into R's brain
# GBIF_plant_lep <- occ_download_import(path ="data/fromR/lfs"
#                                        , key = MD_vasc_and_lep_doi)

# 
# # check out the file
# GBIF_plant_lep
# names(GBIF_plant_lep)
# MD_vasc_lep <- bind.gbif(GBIF_plant_lep)
# after a bit of exploration I feel good about not going with this process.
# instead unzip and just grab the occurrence data themselves for now
# unzip("data/fromR/lfs/0413674-210914110416597.zip", exdir = "data/fromR/lfs/GBIF_downloads")
MD_plant_lep_occ <- data.table::fread("data/fromR/lfs/GBIF_downloads/occurrence.txt")
# # note this returns a warning about parsing th efile (is it weird it gets demonic after line 666?)
head(MD_plant_lep_occ)
MD_occ <- MD_plant_lep_occ %>% filter(taxonRank == "SPECIES")
head(MD_occ)

# # get it so search returns <1e5 results
# 
# 
# MD_vasc <- occ_search(taxonKey = 7707728
#                       , stateProvince = "Maryland", year="2002, 2021"
#                       , basisOfRecord = c("OBSERVATION", "HUMAN_OBSERVATION", "PRESERVED_SPECIMEN")
#                       , hasCoordinate = T
#                       , limit = 1e5)
# 
# mdVascOld <- occ_search(taxonKey = 7707728
#                         , stateProvince = "Maryland", year="1989, 2001"
#                         , basisOfRecord = c("OBSERVATION", "HUMAN_OBSERVATION", "PRESERVED_SPECIMEN")
#                         , hasCoordinate = T
#                         , limit = 1e5)
# 
# 
# 
# md_vasc_obs <- bind_rows(bind.gbif(MD_vasc), bind.gbif(mdVascOld))
# 
# #histogram of species frequency by year
# # md_vasc_obs %>%
# #   group_by(acceptedTaxonKey, year) %>%
# #   summarize(obs =n()) %>%
# #   ggplot(aes(obs))+
# #   geom_histogram()+
# #   facet_wrap(~year)+
# #   theme_classic()+
# #   labs(y="species", x="occurrences")+
# #   scale_y_log10()


# get plant names from state

# plants_gs<-md_vasc_obs %>%
#   separate(acceptedScientificName, sep =" " # this is the accepted name (i.e. most taxonomic issues resolved)
#            , into =c("genus", "species")) %>% # separate just drops stuff after the first two!
#   mutate(gs = paste(genus, species, sep = "_")
#          , withspace = paste(genus, species, sep = " ")) #convenient to keep track of binomials in several forms?

occ_gs <- MD_occ %>%
  separate(acceptedScientificName, sep =" " # this is the accepted name (i.e. most taxonomic issues resolved)
           , into =c("genus", "species")) %>% # separate just drops stuff after the first two!
  mutate(gs = paste(genus, species, sep = "_")
         , withspace = paste(genus, species, sep = " ")) #convenient to keep track of binomials in several forms?

unique(occ_gs$gs)

unique(occ_gs$species)

# obsolete cleanup

# occ_gs %>% filter(species =="X")
# 
# occ_g<-occ_gs %>% filter(!grepl("\\.", .$species))




# write gbif data to file (make these steps modular since they take a long time)
write.csv(occ_gs, "data/fromR/lfs/occ_from_gbif.csv", row.names = F)

occ_gs <- read.csv("data/fromR/lfs/occ_from_gbif.csv")

#get a list of all binomials in the GBIF dataset
plants_of_MD<-unique(occ_gs %>%  filter(kingdomKey == 6) %>% pull(withspace)) #2695  with new data. 

###################################################

#next, download status classifications from natureserve
plant_stats <- ns_search_spp(species_taxonomy = list(scientificTaxonomy = "Plantae", level = "kingdom")
                             , location = list(nation ="US", subnation ="MD") #this filters to only include species that have a MD status, but retains status for all localities
                             , page = 0
                             , per_page = 5e3)[[1]] %>%
  unnest(cols=nations) %>%
  unnest (cols = "subnations", names_repair ="unique") %>%
  filter(subnationCode == "MD") #here is the step where I drop other localities, but this could be dropped at some point.

#merge status and occcurence one kingdom at a time. 
lep_stats <-ns_search_spp(species_taxonomy = list(
  scientificTaxonomy = "Lepidoptera", level = "order",
  kingdom = "Animalia")
  , location = list(nation ="US", subnation ="MD") #this filters to only include species that have a MD status, but retains status for all localities
  , page = 0
  , per_page = 5e3)[[1]] %>%
  unnest(cols=nations) %>%
  unnest (cols = "subnations", names_repair ="unique") %>%
  filter(subnationCode == "MD") #here is the step where I drop other localities, but this could be dropped at some point.

# drafted to deal with bad species names... will see if needed in the post-gis dataset
# ps1<-plant_stats %>% separate(scientificName, sep =" "
#          , into =c("genus", "species")) %>% # separate just drops stuff after the first two!
#   mutate(gs = paste(genus, species, sep = "_")
#          , withspace = paste(genus, species, sep = " "))
# ps2 <- ps1 %>% filter(species %ni% c("x", "var", " ", "")

##############################
##### write and read NS data 

# write just the natureserve data to file
write.csv(plant_stats[,-21], "data/fromR/lfs/plant_NS_data.csv", row.names = F)
write.csv(lep_stats[,-21], "data/fromR/lfs/lep_NS_data.csv", row.names = F)

# read in archived data
lepstat<-read.csv("data/fromR/lfs/lep_NS_data.csv")
plant_stats<-read.csv("data/fromR/lfs/plant_NS_data.csv") 
occ_gs <- read.csv("data/fromR/lfs/occ_from_gbif.csv")





lep_joined <- occ_gs %>% 
  filter(kingdomKey == 1)
  left_join(lepstat, by=c("withspace"="scientificName")) %>% 
  dplyr::filter((!exotic...15 & !exotic...17) |(is.na(exotic...15) & is.na(exotic...17))) %>% 
  mutate(simple_status =ifelse(roundedSRank %in% c("S1", "S2", "S3", "SH"), "threat"
                               , ifelse(roundedSRank %in% c("S4", "S5"), "secure"
                                        , "unranked"))) 

withstats<-plants_gs_new %>%
  select(-gs) %>%
  left_join(plant_stats[,-21], by=c("withspace"="scientificName"))



# summary data
# plant_stats %>% mutate(simple_status = factor(if_else(roundedSRank %in% c("S4","S5"), "secure"
#                                                          , if_else(roundedSRank %in% c("S1", "S2", "S3", "SH"), "threatened", "NONE")))) %>%
#   group_by(simple_status) %>% summarize(n())


# don't delete info, but here is where we can simplify status (also an e.g.)
withstats2<-withstats %>%
  mutate(simple_status =ifelse(roundedSRank %in% c("S1", "S2", "S3", "SH"), "threat"
                               , ifelse(roundedSRank %in% c("S4", "S5"), "secure"
                                        , "unranked")))


data.table::fwrite(withstats2, "data/fromR/lfs/plants_with_status_long.csv", row.names = F)




withstats2<-read.csv("data/fromR/lfs/plants_with_status.csv")
knapp_backboned <- read.csv("data/knapp_backboned.csv")

ws2<-withstats2 %>% 
  mutate(accepted_gs = paste(genus, species, sep = " ")) %>% 
  left_join(knapp_backboned %>% 
              select(-c("scientificName", "kingdom", "phylum", "order"
                        , "family", "kingdomKey", "phylumKey", "classKey"
                        , "orderKey", "familyKey", "genusKey", "speciesKey"
                        , "class")), by ="accepted_gs")

native_plant_stats<-ws2 %>%
  group_by(genus, species, speciesKey) %>% 
  filter(!exclude | has_nonNative_ssp)
excluded_plant_stats <- ws2 %>% 
  group_by(genus, species, speciesKey) %>% 
  filter(exclude & !has_nonNative_ssp)
length(native_plant_stats$decimalLatitude)
length(excluded_plant_stats$decimalLatitude)



write.csv(native_plant_stats, "data/fromR/lfs/kept_plants_with_stats_new.csv", row.names = FALSE)
write.csv(excluded_plant_stats, "data/fromR/lfs/excluded_plants.csv_new", row.names = FALSE)

# data summary
native_plant_stats <- read.csv("data/fromR/lfs/kept_plants_with_stats.csv")


# might not help but for now, overwrite withstats2
withstats2 <- native_plant_stats

############################################
# data exploration
#how many species, families, and records are their in each year by conservation status?
plants_summary<-withstats2 %>%
  group_by(year,state_status = simple_status) %>%
  summarize(spp =n_distinct(speciesKey)
            , families =n_distinct(familyKey)
            , records= n() )



#plot species occurrences per year
sppy<-plants_summary %>%
  ggplot(aes(year, spp, color = state_status))+
  geom_line()+
  theme_classic()+
  scale_y_log10()

#plot families per year
fpy<-plants_summary %>%
  ggplot(aes(year, families, color = state_status))+
  geom_line()+theme_classic()+
  scale_y_log10()

#plot occurrences per year
opy<-plants_summary %>%
  ggplot(aes(year, records, color = state_status))+
  geom_line()+
  theme_classic()+
  scale_y_log10()


#make a figure with these summaries
pdf(file ="figures/GBIF_MD_native_plant_observations.pdf")
sppy/fpy/opy
dev.off()

#look at how many of the species are seen >1, >10x
specfreq<-withstats2 %>%
  group_by(year, state_status = simple_status, species) %>%
  summarize(records =n()) %>%
  group_by(year, state_status) %>%
  summarize(gt1=sum(records>1)/n(), gt10=sum(records>10)/n())

#plot for >1x
once<-specfreq %>%
  ggplot(aes(year,gt1, color = state_status ))+
  geom_line()+theme_classic()+
  labs(y = "proportion spp obs > 1x")

#plot for >10x
tenx<-specfreq %>%
  ggplot(aes(year,gt10, color = state_status ))+
  geom_line()+
  theme_classic()+
  labs(y = "proportion spp obs > 10x")

#save plot to .pdf
pdf(file = "figures/GBIF_MD_native_taxon_freq.pdf")
once/tenx
dev.off()

#get some overall stats
specfreq_TOT<-withstats2 %>%
  group_by( state_status = simple_status, withspace) %>%
  summarize(records =n()) %>%
  group_by( state_status) %>%
  summarize(gt1=sum(records>1)/n(), gt10=sum(records>10)/n(), spp=n())

specfreq_TOT

# look at excluded spp
native_plant_stats %>% 
  group_by(state_status = simple_status) %>% 
  summarize(records = n_distinct(gs))
  
excluded_plant_stats %>% 
  group_by(state_status = simple_status) %>% 
  summarize(records = n_distinct(gs))

