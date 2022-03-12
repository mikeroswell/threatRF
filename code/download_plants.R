#get plant occurrences and statuses

library(natserv) #this is how we get naturserve status
library(rgbif) #how we get gbif observations
library(tidyverse)
library(patchwork)


# when downloading data from gbif, will need to combine different record types into DF
bind.gbif<-function(gbif){bind_rows(gbif[[2]][[3]], gbif[[3]][[3]])}

#deal with complex species name rules with a fucntion to grab just the binomials
uscore_binomial<-function(x){gsub("+ + *", "+_+",x)}

`%ni%` <- Negate(`%in%`) #convenience, this should be part of base R!

###############################################
# get plant occurrence data from gbif
# get it so search returns <1e5 results
MD_vasc <- occ_search(taxonKey = 7707728
                      , stateProvince = "Maryland", year="2002, 2021"
                      , basisOfRecord = c("OBSERVATION", "HUMAN_OBSERVATION", "PRESERVED_SPECIMEN")
                      , hasCoordinate = T
                      , limit = 1e5)

mdVascOld <- occ_search(taxonKey = 7707728
                        , stateProvince = "Maryland", year="1989, 2001"
                        , basisOfRecord = c("OBSERVATION", "HUMAN_OBSERVATION", "PRESERVED_SPECIMEN")
                        , hasCoordinate = T
                        , limit = 1e5)



md_vasc_obs <- bind_rows(bind.gbif(MD_vasc), bind.gbif(mdVascOld))

#histogram of species frequency by year
# md_vasc_obs %>%
#   group_by(acceptedTaxonKey, year) %>%
#   summarize(obs =n()) %>%
#   ggplot(aes(obs))+
#   geom_histogram()+
#   facet_wrap(~year)+
#   theme_classic()+
#   labs(y="species", x="occurrences")+
#   scale_y_log10()


# get plant names from state

plants_gs<-md_vasc_obs %>%
  separate(acceptedScientificName, sep =" "
           , into =c("genus", "species")) %>% # separate just drops stuff after the first two!
  mutate(gs = paste(genus, species, sep = "_")
         , withspace = paste(genus, species, sep = " ")) #convenient to keep track of binomials in several forms?

plants_gs<-plants_gs %>% filter(!grepl("\\.", .$species))

# write gbif data to file (make these steps modular since they take a long time)
write.csv(plants_gs, "data/fromR/lfs/plants_direct_from_gbif.csv", row.names = F)

# plants_gs <- read.csv("data/fromR/lfs/plants_direct_from_gbif.csv")

#get a list of all binomials in the GBIF dataset
plants_of_MD<-unique(plants_gs$withspace) #2620 1999-2021

###################################################

#next, download status classifications from natureserve
plant_stats <- ns_search_spp(species_taxonomy = list(scientificTaxonomy = "Plantae", level = "kingdom")
                             , location = list(nation ="US", subnation ="MD") #this filters to only include species that have a MD status, but retains status for all localities
                             , page = 0
                             , per_page = 5e3)[[1]] %>%
  unnest(cols=nations) %>%
  unnest (cols = "subnations", names_repair ="unique") %>%
  filter(subnationCode == "MD") #here is the step where I drop other localities, but this could be dropped at some point.

#write just the natureserve data to file
write.csv(plant_stats[,-21], "data/fromR/lfs/plant_NS_data.csv", row.names = F)

# plant_stats<-read.csv("data/fromR/lfs/plant_NS_data.csv") #generates an error, weird.

#merge status and occcurrence
withstats<-plants_gs %>%
  select(-gs) %>%
  left_join(plant_stats[,-21], by=c("withspace"="scientificName"))

# don't delete info, but here is where we can simplify status (also an e.g.)
withstats2<-withstats %>%
  mutate(simple_status =ifelse(roundedSRank %in% c("S1", "S2", "S3", "SH"), "threat"
                               , ifelse(roundedSRank %in% c("S4", "S5"), "secure"
                                        , "unranked")))


data.table::fwrite(withstats2, "data/fromR/lfs/plants_with_status.csv", row.names = F)



ns<-read.csv('data/fromR/lfs/plant_NS_data.csv')
gbif<-read.csv("data/fromR/lfs/plants_direct_from_gbif.csv")
str(ns)

gbif %>% summarize(n_distinct(gs))

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
pdf(file ="figures/GBIF_plant_observations.pdf")
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
pdf(file = "figures/GBIF_taxon_freq.pdf")
once/tenx
dev.off()

#get some overall stats
specfreq_TOT<-withstats2 %>%
  group_by( state_status = simple_status, species) %>%
  summarize(records =n()) %>%
  group_by( state_status) %>%
  summarize(gt1=sum(records>1)/n(), gt10=sum(records>10)/n(), spp=n())



