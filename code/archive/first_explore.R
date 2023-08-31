######## HOW MANY BEES ARE ON IUCN (TOTAL, NOT STRENONITIDAE)

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
