######## HOW MANY BEES ARE ON IUCN (TOTAL, NOT STRENONITIDAE)

library(tidyverse)

assmnt <- read.csv("data/IUCNassessments.csv")
ids<- read.csv("data/IUCNtaxonomy.csv")

head(assmnt)
head(ids)

is_assessed<-left_join(assmnt, ids, by ="internalTaxonId")
is_assessed %>% filter(familyName %in% c("ANDRENIDAE", "APIDAE", "COLLETIDAE", "HALICTIDAE", "MEGACHILIDAE", "MELITTIDAE"), realm %in% c("Nearctic", "Nearctic|Neotropical", "Nearctic|Palearctic")) %>% group_by(genusName) %>% summarize(genera=n_distinct(genusName), tot =n())

assmnt %>% group_by(realm) %>% summarize(n())

