#get plant and lepidopteran occurrences and statuses

# this script depends on code/tidy_flora.R
# source("code/tidy_flora.R)
# use own copy of natserv package until PR is accepted or other patch solve issue
devtools::install_github("mikeroswell/natserv-1", force = TRUE)
library(natserv) #this is how we get natureserve status
library(rgbif) #how we get gbif observations
library(dplyr)
library(purrr)
library(ggplot2)
library(tidyr)
library(patchwork) # cute pkg for assembling plots


# # when downloading data from gbif with `occ_search`
# need to combine different record types into DF
bind.gbif<-function(gbif){bind_rows(gbif[[2]][[3]], gbif[[3]][[3]])}

#deal with complex species name rules with a function to grab just the binomials
uscore_binomial<-function(x){gsub("+ + *", "+_+",x)}

`%ni%` <- Negate(`%in%`) #convenience, this should be part of base R!
# return NA instead of character(0) or NULL
null_to_NA <- function(x){
  c(x, NA)[1]
}


###############################################
# get occurrence data from gbif

# now refactoriing to use occ_downlaod which comes with a doi
# this is the query sent to GBIF via the API to create a downloadable object
# likely takes several minutes.
# does not need to be redone each time code is run

# MD_vasc_and_lep_doi <- occ_download(
#   user = gbif_user
#   , pwd = gbif_pwd
#   , email = gbif_email
#   , pred_in("taxonKey", c(7707728, 797))
#   , pred_in("basisOfRecord", c("OBSERVATION", "HUMAN_OBSERVATION", "PRESERVED_SPECIMEN"))
#   , pred_in("stateProvince", "Maryland")
#   , pred("hasCoordinate", TRUE)
#   # , pred("sendNotification", TRUE)
#   , pred_and(pred_gte("year", 2001), pred_lte("year", 2023))
#   )

# # this reveals status of all gbif download requests, includes DOIs
# occ_download_list( user = gbif_user
#                    , pwd = gbif_pwd
#                   )


# When changed to 2001-2023, got about double the records as previous search, 
# iNaturalist is probably the reason. I don't think this is necessarily a problem
# or even a topic to discuss?

# # this actually downloads the data (~143 MB)
# occ_download_get(MD_vasc_and_lep_doi, path = "data/fromR/lfs", overwrite = TRUE)

# # another function to read into R's brain
# GBIF_plant_lep <- occ_download_import(path ="data/fromR/lfs"
#                                       , key = MD_vasc_and_lep_doi)


# # unzip and grab the occurrence data themselves for now
# # unzip("data/fromR/lfs/0003222-240506114902167.zip", exdir = "data/fromR/lfs/GBIF_downloads")
# MD_plant_lep_occ <- data.table::fread("data/fromR/lfs/GBIF_downloads/occurrence.txt")
# # # note this returns a warning about parsing the file 
# head(MD_plant_lep_occ)
# MD_occ <- MD_plant_lep_occ %>% filter(taxonRank == "SPECIES")
# head(MD_occ)
# 
# 
# # 
# #histogram of species frequency by year
# MD_occ %>%
#   group_by(acceptedTaxonKey, year) %>%
#   summarize(obs =n()) %>%
#   ggplot(aes(obs))+
#   geom_histogram()+
#   facet_wrap(~year)+
#   theme_classic()+
#   labs(y="species", x="occurrences")+
#   scale_y_log10()
# 
# # inaturalist effect so clear starting 2019, it's bonkers
# 
# 
# 
# occ_gs <- MD_occ %>%
#   separate(acceptedScientificName, sep =" " # this is the accepted name (i.e. most taxonomic issues resolved)
#            , into =c("genus", "species")) %>% # separate just drops stuff after the first two!
#   mutate(gs = paste(genus, species, sep = "_")
#          , withspace = paste(genus, species, sep = " ")) %>%  #convenient to keep track of binomials in several forms?
#   filter(!grepl("^[^[:alnum:]]", species)) # remove hybrids (can't deal with them responsibly)
# 
# unique(occ_gs$gs) #4864 spp!
# 
# 
# 
# 
# # write gbif data to file (make these steps modular since they take a long time)
# write.csv(occ_gs, "data/fromR/lfs/occ_from_gbif.csv", row.names = F)

occ_gs <- read.csv("data/fromR/lfs/occ_from_gbif.csv")


#get a list of all plant binomials in the GBIF dataset
plants_of_MD <- unique(occ_gs %>%  filter(kingdomKey == 6) %>% pull(withspace)) 
# 2915; maybe there was a taxonomy update  with new data, otherwise we lost
# hundreds of names in the 90s as it was 3253 before.

###################################################

#next, download status classifications from natureserve
# merge status and occcurence one kingdom at a time. 

# set max page to 41 by trial and error
plant_stats <- map_dfr(0:41, function(pp){
  ns_search_spp(species_taxonomy = list(scientificTaxonomy = "Plantae", level = "kingdom")
                             , location = list(nation ="US", subnation ="MD") #this filters to only include species that have a MD status, but retains status for all localities
                             , page = pp
                             , per_page = 100)[[1]] %>%
  unnest(cols = nations) %>%
  unnest (cols = "subnations", names_repair = "unique") %>%
  filter(subnationCode == "MD") #here is the step where I drop other localities, but this could be dropped at some point.
})

# Note, this only works with my version of natserv, currently
lep_stats <-map_dfr(0:3, function(pp){
  myres <- ns_search_spp(species_taxonomy = list(
    level = "order"
    , scientificTaxonomy = "Lepidoptera"
    , kingdom = "Animalia" # at one point this was necessary but maybe not with current natserve
  )
  , location = list(nation = "US", subnation = "MD") #this filters to only include species that have a MD status, but retains status for all localities
  , page = pp
  , per_page = 100)[[1]] %>%
  unnest(cols = nations) %>%
  unnest (cols = "subnations", names_repair = "unique") %>%
  filter(subnationCode == "MD") #here is the step where I drop other localities, but this could be dropped at some point.
})

# drafted to deal with bad species names
ps1 <- plant_stats %>% 
  separate(scientificName
            , sep =" "
            , into =c("genus", "species")
            , remove = FALSE) %>% # separate just drops stuff after the first two!
  mutate(gs = paste(genus, species, sep = "_")
         , withspace = paste(genus, species, sep = " ")) %>%
  filter(species %ni% c("x", "var", " ", "") & !grepl("^[^[:alnum:]]", species)) %>% 
  select(-speciesGlobal)

ls1 <- lep_stats %>% 
  separate(scientificName
             , sep =" "
             , into =c("genus", "species")
             , remove = FALSE) %>% # separate just drops stuff after the first two!
  mutate(gs = paste(genus, species, sep = "_")
         , withspace = paste(genus, species, sep = " ")) %>%
  filter(species %ni% c("x", "var", " ", "") & !grepl("^[^[:alnum:]]", species))%>% 
  select(-speciesGlobal)


# need to do some name cleanup on the NS data, unfortunately
source("code/robust_gbif_namesearch.R")

nslnames <- ls1 %>% 
  mutate(gs = paste(genus, species)) %>% 
  pull(gs) %>% 
  unique() %>% 
  sapply(function(x){
    nbRobust(x, kingdom = "Animalia" )
  }) %>% 
  map_dfr(bind_rows, .id = "gs")

problem_names_lep <- nslnames %>% 
  filter(is.na(matchType) | matchType == "HIGHERRANK" | status == "SYNONYM") %>%
  pull(gs)

dat <- ls1 %>% 
  filter(withspace %in% problem_names_lep) %>% 
  as.data.frame()

alt_gs <- function(dat){
  map_dfr(1:nrow(dat)[[1]]
          , function(lrow){
            # get the ns taxon concept info
            ns_deets <- natserv::ns_altid(as.character(dat[lrow, "uniqueId"]))
            # start with itis
            itis.name <- null_to_NA(
              stringr::str_replace(ns_deets$relatedItisNames
                                   , "(.*\\<i\\>)(.*)(\\<\\/i\\>.*)"
                                   , "\\2"))
            
            # if itis doesn't work, try conceptName
            better.name <- if_else(is.na(itis.name)
                                   , null_to_NA(
                                     stringr::str_replace(ns_deets$conceptName
                                                          , "(.*\\<i\\>)(.*)(\\<\\/i\\>.*)"
                                                          , "\\2"))
                                   , itis.name)
            
          
            
            # goal is to line up with gbif backbone 
            gbif.name <- rgbif::name_backbone(better.name)
            
            natserv.name <- dat[lrow, "withspace"]
            
            return(data.frame(natserv.name, gbif.name))
          })
}


better_leps <- alt_gs(ls1 %>% 
                        filter(withspace %in% problem_names_lep) %>% 
                        as.data.frame()) %>% 
  select(gbif.gs = species, ns.gs = natserv.name)

# notes: Looks like almost all are 1:1 synonymies (good!)
# exceptions:

# - Peridea bordeloni is a new species not yet recognized by ITIS/ GBIF. The
# parent species (P. ferruginea) is not ranked in MD, so won't affect results
# here.

better_leps <- better_leps %>% filter(!is.na(gbif.gs))


nspnames <- ps1 %>% 
  mutate(gs = paste(genus, species)) %>% 
  pull(gs) %>% 
  unique() %>% 
  sapply(function(x){
    nbRobust(x, kingdom = "Plantae" )
  }) %>% 
  map_dfr(bind_rows, .id = "gs")

# # data quality checks
# nspnames %>% filter(genus == "Trautvetteria")
# nspnames %>% filter(genus == "Ulmus")

problem_names_plant <- nspnames %>% 
  filter(matchType != "EXACT" | status == "SYNONYM") %>% 
  pull(gs)

problem_names_plant

dat <- ps1 %>% 
  filter(withspace %in% problem_names_plant) %>% 
  as.data.frame()


better_plants <- alt_gs(ps1 %>% 
                        filter(withspace %in% problem_names_plant) %>% 
                        as.data.frame()) %>% 
  select(gbif.gs = species, ns.gs = natserv.name)





# Some mismatches, enumerate those that aren't SNR

# Hexalectris spicata = Bletia spicata (this one looks like it's matching correctly rn, need to do first)
# Listera cordata = Neottia cordata
# Piptatherum racemosum = Patis racemosa
# Saccharum brevibarbe = Erianthus 
# Saccharum contortum = Erianthus
# Oldenlandia uniflora = Edrastema
# Oligoneuron rigidum  = Solidago
# Onosmodium molle  = Lithospermum parviflorum
# Polygonum amphibium = Persicaria amphibia
# Polygonum lapathifolium = Persicaria lapathifolia
# Rhamnus alnifolia = Endotropis alnifolia
# Trautvetteria caroliniensis = T carolinensis


plant_stats %>% filter(scientificName %in% 
                         (better_plants %>% 
                            group_by(ns.gs)
                          %>% filter(all(is.na(gbif.gs))) 
                          %>% pull(ns.gs)) 
) %>% 
  filter(roundedSRank != "SNR") %>% 
  select(scientificName, roundedSRank) %>% 
  filter(roundedSRank != "SNA")
# there are 16 spp (do I need to filter those? Probably!)
# looks like a little more than half are non-native


ps2 <- ps1 %>% 
  left_join(better_plants, by = c("withspace" = "ns.gs" )) %>%
  group_by(withspace) %>% 
  mutate(withspace = ifelse(any(!is.na(gbif.gs)), gbif.gs,  withspace)
         , gs = gsub(" ", "_", withspace)
         , genus = gsub("(.*)( )(.*)", "\\1", withspace)
         , species = gsub("(.*)( )(.*)", "\\3", withspace)) %>% 
  filter(!grepl("ssp.", scientificName))

# data checks again
ps2 %>% filter(grepl("brevibarbe", withspace)) # still here with its NS name
ps2 %>% filter(grepl("arundinaceus", withspace))

ls2 <- ls1 %>% 
  left_join(better_leps, by = c("withspace" = "ns.gs" )) %>% 
  group_by(withspace) %>% 
  mutate(withspace = ifelse(any(!is.na(gbif.gs)), gbif.gs,  withspace)
         , gs = gsub(" ", "_", withspace)
         , genus = gsub("(.*)( )(.*)", "\\1", withspace)
         , species = gsub("(.*)( )(.*)", "\\3", withspace)) %>% 
  filter(!grepl("ssp.", scientificName))

##############################
##### write and read NS data 

# write just the natureserve data to file
write.csv(ps2, "data/fromR/lfs/plant_NS_data.csv", row.names = F)
write.csv(ls2, "data/fromR/lfs/lep_NS_data.csv", row.names = F)

# read in archived data
ps2 <- read.csv("data/fromR/lfs/plant_NS_data.csv")
ls2 <- read.csv("data/fromR/lfs/lep_NS_data.csv")
# lep_stats <- read.csv("data/fromR/lfs/lep_NS_data.csv")
# plant_stats <- read.csv("data/fromR/lfs/plant_NS_data.csv") 
occ_gs <- read.csv("data/fromR/lfs/occ_from_gbif.csv")
knapp_backboned <- read.csv("data/knapp_backboned.csv") %>%
  separate(accepted_gs
           , sep =" "
           , into =c("acc_genus", "acc_species")
           , remove = FALSE) %>%
  filter(!grepl("^[^[:alnum:]]", acc_species))


# grep("^[[:punct:]]", occ_gs$species, value = TRUE)

lep_joined <- occ_gs %>% 
  filter(kingdomKey == 1) %>% 
  left_join(ls2, by = c("gs", "genus", "species")) %>% 
  dplyr::filter((!exotic...15 & !exotic...17) |(is.na(exotic...15) & is.na(exotic...17))) %>% 
  mutate(simple_status =ifelse(roundedSRank %in% c("S1", "S2", "S3", "SH"), "threat"
                               , ifelse(roundedSRank %in% c("S4", "S5", "S5B"), "secure"
                                        , ifelse(roundedSRank %in% "SNA", "remove_not_native"
                                        , "unranked"))) )


# occ_gs %>% 
#   filter( genus == 'Trautvetteria') %>% 
#   select(gs, genus, species, withspace)
# 
# ps2 %>% 
#   filter( genus == 'Trautvetteria') %>% 
#   select(gs, genus, species, withspace)
  
occ_stat_plants <- occ_gs %>% 
  filter(kingdomKey == 6) %>% 
  left_join(ps2, by = c("gs", "genus", "species", "withspace")) %>% 
  mutate(simple_status =ifelse(roundedSRank %in% c("S1", "S2", "S3", "SH"), "threat"
                               , ifelse(roundedSRank %in% c("S4", "S5"), "secure"
                                        , ifelse(roundedSRank %in% "SNA", "remove_not_native"
                                        , "unranked"))))
plant_joined <- occ_stat_plants %>% 
  left_join(knapp_backboned  %>% 
              select(-c("scientificName", "kingdom", "phylum", "order"
                        , "family", "kingdomKey", "phylumKey", "classKey"
                        , "orderKey", "familyKey", "genusKey", "speciesKey"
                        , "class", "gs")), by = c("withspace" = "accepted_gs"))  

# check species not in flora
plant_joined %>%
  filter(is.na(verbiage) #&! grepl("fl", gs) &! grepl("fi", gs)
         ) %>%
  group_by(gs) %>%
  summarize(recs = n()) %>%
  filter(recs >2) %>%
  arrange(desc(recs)) %>% View()

# it seems like there are a bunch of things going on, one of which is spellings (like why is accepted )

# Trautvetteria carolinensis = T caroliniensis looks like a yikes.
#  
all_with_stat<- bind_rows(lep_joined, plant_joined)
# 
# 
# # summary data
# # plant_stats %>% mutate(simple_status = factor(if_else(roundedSRank %in% c("S4","S5"), "secure"
# #                                                          , if_else(roundedSRank %in% c("S1", "S2", "S3", "SH"), "threatened", "NONE")))) %>%
# #   group_by(simple_status) %>% summarize(n())
# 
# 
# # don't delete info, but here is where we can simplify status (also an e.g.)
# data.table::fwrite(all_with_stat, "data/fromR/lfs/occ_with_status_long.csv", row.names = FALSE)

all_with_stat <- data.table::fread("data/fromR/lfs/occ_with_status_long.csv")
# all_with_stat %>% filter(grepl("^[^[:alnum:]]", species))
# all_with_stat %>% filter(grepl("[^[:alnum:][:punct:]]", species)) %>% select (genus, species, gs)

natives_filtering <- all_with_stat %>%
  # deal with spp not in Knapp

  filter((kingdomKey == 6 & ( verb_length > 0
  )
  )|kingdomKey ==1) %>%
  group_by(genus, species, speciesKey) %>%
  filter(all(!exclude) |has_nonNative_ssp | is.na(exclude)) %>%
  filter(!(grepl("campestre", species) & grepl("Acer", genus))) %>%
  filter((!exotic...239| is.na(exotic...239)), (!exotic...241|is.na(exotic...241)))

natives_filtering %>%
  filter(grepl("japonic", species
  )) %>%
  select(genus, species, verbiage, exclude, has_nonNative_ssp)

natives <- all_with_stat %>%
   # deal with spp not in Knapp

  filter((kingdomKey == 6 & ( verb_length > 0
                              )
          )|kingdomKey ==1) %>% 
  group_by(genus, species, speciesKey) %>%
  filter(!exclude | has_nonNative_ssp | is.na(exclude)) %>%
  filter(!(grepl("campestre", species) & grepl("Acer", genus))) %>% 
  filter((!exotic...239| is.na(exotic...239)), (!exotic...241|is.na(exotic...241))) %>%
  filter(roundedSRank!= "SNA") %>% 

  select(gbifID
         , genus
         , species
         , speciesKey
         , decimalLatitude
         , decimalLongitude
         , kingdomKey
         , family
         , order
         , roundedSRank
         , simple_status) %>% 
  ungroup() %>% 
  group_by(genus, species) %>% 
  # note here that we use a greedy classification: if any infra-species or 
  # synonym associated with a valid name is threatened, we use this status for 
  # the whole species. 
  mutate(simple_status = if_else("threat" %in% simple_status, "threat"
        , if_else("secure" %in% simple_status, "secure", "unranked"))) %>% 
  group_by(genus, species, simple_status, decimalLongitude, decimalLatitude) %>% 
  slice_head(n = 1) 



natives %>%
  ungroup() %>% 
  mutate(gs = paste(genus, species)) %>%
  group_by(kingdomKey, simple_status) %>% 
  summarize(occ = n(), spp = n_distinct(gs)) 


# natives %>%
#   sf::st_drop_geometry() %>% 
#   group_by(genus, species, simple_status) %>% 
#   summarize(locs = n_distinct(decimalLatitude), sranks = n_distinct(roundedSRank), tot = n()) %>% 
#   filter(species == "pauciflora")

# # look at plants since I have a feel for them
# natives %>%
#   filter(kingdomKey == 6) %>%
#   group_by(genus, species) %>%
#   summarize(n()) %>%
#   filter(genus == "Acer")

# natives %>% filter(grepl("japonic", species))


excludeds <- all_with_stat %>%
  group_by(genus, species, speciesKey)  %>% 
  filter((exclude & !has_nonNative_ssp) | exotic...239 |exotic...241 |
           !(kingdomKey == 6 & ( verb_length  >0            )
           ) | (grepl("campestre", species) & grepl("Acer", genus)) |
           roundedSRank == "SNA") %>%
  select(gbifID
         , genus
         , species
         , speciesKey
         , decimalLatitude
         , decimalLongitude
         , kingdomKey
         , family
         , order
         , roundedSRank
         , simple_status) %>% 
  ungroup() %>% 
  # note here that we use a greedy classification: if any infra-species or 
  # synonym associated with a valid name is threatened, we use this status for 
  # the whole species. 
  mutate(simple_status = if_else("threat" %in% simple_status, "threat"
                                 , if_else("secure" %in% simple_status
                                           , "secure", "unranked"))) %>% 
  group_by(genus, species, simple_status, decimalLongitude, decimalLatitude) %>% 
  slice_head(n = 1) 
  


# all_with_stat %>% group_by(genus, species) %>% summarize(n()) %>% filter(genus == "Acer")
# 
# occ_gs %>% filter(genus == "Acer") %>% group_by(genus, species) %>% summarize(n())

# excludeds %>%
#   filter(kingdomKey == 6) %>%
#   group_by(genus, species) %>%
#   summarize(n()) %>%
#   filter(genus == "Acer")
# # 

data.table::fwrite(natives, "data/fromR/lfs/native_records.csv", row.names = FALSE)
data.table::fwrite(excludeds, "data/fromR/lfs/excluded_records.csv", row.names = FALSE)

# # data summary
# native_plant_stats <- read.csv("data/fromR/lfs/kept_plants_with_stats.csv")
# 
# 
# # might not help but for now, overwrite withstats2
# withstats2 <- native_plant_stats
# 
# ############################################
# # data exploration
# #how many species, families, and records are their in each year by conservation status?
# plants_summary<-withstats2 %>%
#   group_by(year,state_status = simple_status) %>%
#   summarize(spp =n_distinct(speciesKey)
#             , families =n_distinct(familyKey)
#             , records= n() )
# 
# 
# 
# #plot species occurrences per year
# sppy<-plants_summary %>%
#   ggplot(aes(year, spp, color = state_status))+
#   geom_line()+
#   theme_classic()+
#   scale_y_log10()
# 
# #plot families per year
# fpy<-plants_summary %>%
#   ggplot(aes(year, families, color = state_status))+
#   geom_line()+theme_classic()+
#   scale_y_log10()
# 
# #plot occurrences per year
# opy<-plants_summary %>%
#   ggplot(aes(year, records, color = state_status))+
#   geom_line()+
#   theme_classic()+
#   scale_y_log10()
# 
# 
# #make a figure with these summaries
# pdf(file ="figures/GBIF_MD_native_plant_observations.pdf")
# sppy/fpy/opy
# dev.off()
# 
# #look at how many of the species are seen >1, >10x
# specfreq<-withstats2 %>%
#   group_by(year, state_status = simple_status, species) %>%
#   summarize(records =n()) %>%
#   group_by(year, state_status) %>%
#   summarize(gt1=sum(records>1)/n(), gt10=sum(records>10)/n())
# 
# #plot for >1x
# once<-specfreq %>%
#   ggplot(aes(year,gt1, color = state_status ))+
#   geom_line()+theme_classic()+
#   labs(y = "proportion spp obs > 1x")
# 
# #plot for >10x
# tenx<-specfreq %>%
#   ggplot(aes(year,gt10, color = state_status ))+
#   geom_line()+
#   theme_classic()+
#   labs(y = "proportion spp obs > 10x")
# 
# #save plot to .pdf
# pdf(file = "figures/GBIF_MD_native_taxon_freq.pdf")
# once/tenx
# dev.off()
# 
# #get some overall stats
# specfreq_TOT<-withstats2 %>%
#   group_by( state_status = simple_status, withspace) %>%
#   summarize(records =n()) %>%
#   group_by( state_status) %>%
#   summarize(gt1=sum(records>1)/n(), gt10=sum(records>10)/n(), spp=n())
# 
# specfreq_TOT
# 
# # look at excluded spp
# native_plant_stats %>% 
#   group_by(state_status = simple_status) %>% 
#   summarize(records = n_distinct(gs))
#   
# excluded_plant_stats %>% 
#   group_by(state_status = simple_status) %>% 
#   summarize(records = n_distinct(gs))
# 

