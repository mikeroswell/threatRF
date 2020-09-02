# do some data cleanup

library(data.table)
library(tidyverse)
library(RMariaDB) #This is most up to date package for linking to databases. RMySQL also works well but
# source("psw_clab.R") #simply save a .R file as psw="PASSWORD"
# source("user_clab.R")
#
#
# conn <- dbConnect(RMariaDB::MariaDB(), user=user_clab, password=psw_clab,
#                   dbname="cariveau_lab", port = 8889 , host= "160.94.186.138")
#
# dbListTables(conn)
# traits = dbReadTable(conn, "species_traits")
# dbDisconnect(conn)
#
# traits$gs <- paste(traits$genus, traits$species, sep=" ")
# write.csv(traits, "data/fromR/traits.csv")

traits<-read.csv("data/fromR/traits.csv")

beeDat<-fread("data/fromR/ObsMDBeesSamGBIFConservStat.csv")
project<-beeDat

cleannames <- function(project, binomial_column = "gs"){
  project = project  %>% rename_("gs" = binomial_column) %>% mutate(gs = str_to_sentence(gs)) %>% # deal with capitalization errors
    separate(col = "gs", into = c("genus", "species"), sep = " ")
  project$species[project$genus=="Melissodes" & project$species== "bimaculata" ] <- "bimaculatus"
  project$species[project$genus=="Melissodes" & project$species== "denticulata" ] <- "denticulatus"
  project$species[project$genus=="Melissodes" & project$species== "druriella" ] <- "druriellus"
  project$species[project$genus=="Melissodes" & project$species== "illata" ] <- "illatus"
  project$species[project$genus=="Melissodes" & project$species== "subillata" ] <- "subillatus"
  project$species[project$genus=="Melissodes" & project$species== "desponsa" ] <- "desponsus"
  project$species[project$genus=="Melissodes" & project$species== "nivea" ] <- "niveus"
  project$species[project$genus=="Lasioglossum" & project$species== "illinoensis" ]<- "illinoense"
  project$species[project$genus=="Nomada" & project$species== "luteolodies" ]<- "luteoloides"
  project$species[project$genus=="Dufouria" & project$species == "novaeangleae"] <- "novaeangliae"
  project$genus[project$genus=="Dufouria"] <- "Dufourea"
  project$species[project$genus=="Coelioxys" & project$species == "germana"] <- "germanus"
  project$species[project$genus=="Coelioxys" & project$species == "alternata"] <- "alternatus"
  project$species[project$genus=="Coelioxys" & project$species == "modesta"] <- "modestus"
  project$species[project$genus=="Coelioxys" & project$species == "octodentata"] <- "octodentatus"
  project$species[project$genus=="Coelioxys" & project$species == "moesta"] <- "moestus"
  project$species[project$genus=="Coelioxys" & project$species == "funeraria"] <- "funerarius"
  # double check why this one Coelioxys is messed up
  # project$species[project$genus=="Coelioxys" & project$species == "immaculata"] <- "immaculatus"
  project$species[project$genus=="Coelioxys" & project$species == "immaculatus"] <- "immaculata"
  project$species[project$genus=="Sphecodes" & project$species == "carolinus"] <- "coronus"
  project$species[project$genus=="Sphecodes" & project$species == "antennaria"] <- "antennariae"
  project$species[project$genus=="Sphecodes" & project$species == "johnsoni"] <- "johnsonii"
  project$genus[project$genus=="Peponapis"] <- "Eucera"

  project$species[project$genus=="Megachile" & project$species == "rotunda"] <- "rotundata"

  project$species[project$genus=="Heriades" & project$species=="carinata"]<- "carinatus"
  project$species[project$genus=="Heriades" & project$species=="variolosa"]<- "variolosus"

  ### collapse L. oceanicum synonyms
  project$species[project$species=="nymphaearum"]<-"oceanicum"
  ####################################################
  #these are taxonomy decisions


  #I think it's reasonable to switch this name
  project$species[project$genus=="Lasioglossum" & project$species== "divergens" ] <- "macoupinense"
  project$species[project$genus=="Pseudopanurgus" & project$species== "nebrascensis" ] <- "aestivalis"
  project$species[project$genus=="Lasioglossum" & project$species== "perplexa" ] <- "perplexans"
  #I think merging these two species makes sense for analysis b/c male is undescribed for weemsi, not clear even to J. Gibbs that these are truly distinct spp. Also we have lots that are entered on SQL without determination between these two.
  project$species[project$genus=="Lasioglossum" & project$species== "hitchensi/weemsi" ] <- "hitchensi_weemsi"
  project$species[project$genus=="Lasioglossum" & c(project$species=="hitchensi" |project$species=="weemsi")]<- "hitchensi_weemsi"
  project$species[project$genus=="Lasioglossum" & c(project$species=="mitchelli" |project$species=="weemsi")]<- "hitchensi_weemsi"

  #this is more of a judgement call... not a naming thing
  project$species[project$genus=="Nomada" & project$species== "near_lehighensis" ]<- "lehighensis"

  #lump things in Hylaeus affinis/modestus group (now including illinoisensis(along with annulatus? into one taxon)
  project$species[project$genus=="Hylaeus" & c(project$species=="annulatus_or_mod"|project$species=="near_modestus"|project$species=="near_affinis" |project$species=="affinis" |project$species=="modestus" |project$species=="affinis_like" | project$species== "affinis/modestus"| project$species== "modestus/affinis"|project$species== "modestus-affinis"|project$species== "affinis-modestus"|project$species== "annulatus_modestus"| project$species== "modestus_like") |project$species== "modestus group sp. 1"|project$species== "modestus group sp. 2" |project$species== "modestus group sp. 3"|project$species== "modestus group sp. 4" |project$species=="illinoisensis"] <- "affinis_modestus"

  #will call poeyi and ligatus a complex
  project$species[project$genus == "Halictus" & c(project$species=="ligatus"|project$species=="poeyi")] <- "ligatus_poeyi"



  #Ceratina not fully resolved in AMNH. Won't drive results for rare b/c none of the species are rare anyways, tho mikmaqi only 1 record in AMNH. One question is whether this is a concern for other groups. We decided to pretend that AMNH calcarata==calcarata and dupla_sensu_lato== dupla for now
  project$species <- gsub("_sensu_lato","",project$species)
  # project$species[project$genus=="Ceratina" & project$species== "calcarata cf C. dupla" ] <- "calcarata_dupla_mikmaqi"
  # project$species[project$genus=="Ceratina" & project$species %in% c(grep("dupl",project$species, value=T),grep("calca",project$species, value=T), grep( "mikma",project$species, value=T))] <- "calcarata_dupla_mikmaqi"


  # Nomada are tough in general. Make a complex with these two named spp.
  project$species[project$genus=="Nomada" & c(project$species=="sayi" |project$species=="illinoensis"|project$species=="sayi_illinoensis")]<- "illinoensis_sayi"

  #bidentate group not in AMNH, but reasonably consistent within lab data?


  #eliminate 1 specimen without determination from molly's
  project<-project %>% filter(species!="asteris_asteroides")

  # View(project %>% group_by(genus, species, project) %>% summarize(n()))

  #bidentate group... perhaps not worse than ceratina_dupla group or Hylaeus_affinis group?
  #From tina's
  project<-project %>%
    filter(genus!="gen" & species!="sp_CHECK"& species!="sp"& species!="lost_spec"& species!="sp1"& species!="sp2"& species!="sp3"& species!="bidentate_gr"& species!="bidentate_group"& species!="interesting_seeTN"& species!="sp_seeTN" & species !="sp_101" & species %in% grep("maybe", .$species, value=T, invert=T))


  project$species <- gsub("__","_",project$species)
  project<-project[grep("calcarata_", project$species, invert=T),]
  project<-project[grep("dupla_", project$species, invert=T),]
  project <- project[ grep("_seeTN", project$species, invert = TRUE) , ]
  project <- project[ grep("_cf", project$species, invert = TRUE) , ]


  #project$species <- gsub("_seeTN","",project$species) # remove _seeTN
  #tina would say drop these _cf bees because they aren't typically the species they are cfing! currently only 1 bee.
  #
  project$gs <- paste(project$genus, project$species, sep=" ")
  return(project)
}

beeDat<-cleannames(beeDat)
beeDat<-beeDat %>% filter(gs!="Andrena ")

beeDat %>% anti_join(traits, by = "gs") %>% group_by(gs) %>% summarize(n())

fwrite(beeDat, "data/FromR/Obs_stat_cleaned.csv")

