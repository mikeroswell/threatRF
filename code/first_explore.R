######## HOW MANY BEES ARE ON IUCN (TOTAL, NOT STRENONITIDAE)

library(tidyverse)
library(rgbif)

assmnt <- read.csv("data/IUCNassessments.csv")
ids <- read.csv("data/IUCNtaxonomy.csv")

head(assmnt)
head(ids)

is_assessed<-left_join(assmnt, ids, by ="internalTaxonId")
is_assessed %>% filter(familyName %in% c("ANDRENIDAE", "APIDAE", "COLLETIDAE", "HALICTIDAE", "MEGACHILIDAE", "MELITTIDAE"), realm %in% c("Nearctic", "Nearctic|Neotropical", "Nearctic|Palearctic")) %>% group_by(genusName) %>% summarize(genera=n_distinct(genusName), tot =n())

assmnt %>% group_by(realm) %>% summarize(n())

######### what is going on with Anahí's previous analysis? Looks like no spp. are listed as secure on MD sheet.

with_status<-data.table::fread("data/ObsMDBeesConservStat.csv")
head(with_status)
View(with_status %>% group_by(name, Conservation.Status) %>% summarize(n()) %>% arrange(desc(Conservation.Status)))


################# get gbif records

get_gbif <- function(taxon
                   , min.lat = 37.8889
                   , max.lat = 39.722
                   , min.lon = -79.4861
                   , max.lon = -74.8581
                   , min.year = 1981
                   , max.year = 2020
                   ){
  occ_data(scientificName = taxon
           , country ="US"
           , hasCoordinate = T
           , year=paste0(min.year, ",", max.year)
           , decimalLatitude = paste0(min.lat, ",", max.lat)
           , decimalLongitude = paste0(min.lon, ",", max.lon)
           , limit = 5e5)
}
test_bif<-get_gbif("Andrena carlini")
test_bif
