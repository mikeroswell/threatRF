# code to tidy untidy flora data
library(dplyr)
library(purrr)
library(tidyr)
library(stringr)
source("code/robust_gbif_namesearch.R")

# to create this file, saved .pdf of only the useful pages to "accessible text" 
# with acrobat
knapp_raw <- read.table("data/Knapp_word_to_Accessible_search_and_replace_emDash_special.txt", header = FALSE, sep = "\t")


knapp_raw <- knapp_raw %>% slice(1:9416)
str(knapp_raw)


# learning data and regex

# grepl("^\\s[A-Z]{1}[a-z]* \\<[a-z]", "Sentence case")

# grepl("[[:blank:]]*^-*[[:upper:]][[:lower:]]* [[:lower:]]*\\>", c("-Yes yes", "Yes yes", "Yes yes yes", "Yes yes YES", "Almost almostAlmost", "no no", "NO no ", "nO", "No No")) # is almost a problem?

# str_extract(c("-Yes yes", "Yes yes", "Yes yes yes", "Yes yes YES", "Almost almostAlmost", "no no", "NO no ", "nO", "No No"), "^-*[[:upper:]][[:lower:]]+ [[:lower:]]{3,")

# grepl("^[[:blank:]]*-*[[:upper:]][[:lower:]]+ [[:lower:]]{3,}\\>", "Bluegrass, ‚Ä† (Kearny 70 US)")
# 
# str_extract(c("test", "your mom", "your mom is the best"), "^[a-z]*")
# add a useful sorting column, fix some of the issues from the "f" in the
# checklist
# note that this stuff *fixes* the genus and species columns, but in many cases
# introduces errors into the associated data (synonyms, etc) which were written
# in the .pdf in a different typefaces.
knapp_next <- knapp_raw %>% 
  mutate(sl = str_length(V1)
         , V1=str_replace(.data$V1, "( d)([a-z]{1,2}) ", "fid\\2 ")
         , V1=str_replace(.data$V1, " avesce", "flavesce")
         , V1=str_replace(.data$V1, " avu", "flavu")
         , V1=str_replace(.data$V1, " exuo", " flexuo")
         , V1=str_replace(.data$V1, " avico", "flavico")
         , V1=str_replace(.data$V1, " uvia", "fluvia")
         , V1=str_replace(.data$V1, " licinus ", "filicinus ")
         , V1=str_replace(.data$V1, " liculm", "filiculm")
         , V1=str_replace(.data$V1, "ru dulum", "rufidulum")
         , V1=str_replace(.data$V1, "ra nesq", "rafinesq")
         , V1=str_replace(.data$V1, " lifor", " filifor")
         , V1=str_replace(.data$V1, "romanzof ", "romanzoffi")
         , V1=str_replace(.data$V1, "( or)([a-z]{1,2}) ", "flor\\2 ")
         , V1=str_replace(.data$V1, "ci ua", "ciflua")
         , V1=str_replace(.data$V1, "Wolf ella", "Wolffiella")
         , V1=str_replace(.data$V1, "Najas exi", "Najas flexi")
         , V1=str_replace(.data$V1, " ava ", " flava ")
         , V1=str_replace(.data$V1, " lamentosa", " filamentosa")
         , V1=str_replace(.data$V1, "rofex", "roflex")
         , V1=str_replace(.data$V1, "accida", "flaccida")
         , V1=str_replace(.data$V1, " exicaulis", " flexicaulis")
         , V1=str_replace(.data$V1, " coidea", " ficoidea")
         , V1=str_replace(.data$V1, " orib", " florib")
         , V1=str_replace(.data$V1, " orid", " florid")
         , V1=str_replace(.data$V1, "( )(ex[a-z]*) ", "fl\\2 ")
         , V1=str_replace(.data$V1, "in rma ", "infirma ")
         , V1=str_replace(.data$V1, "of cinalis ", "officinalis ")
         , V1=str_replace(.data$V1, "proli cum", "prolificum")
         , V1=str_replace(.data$V1, " stulo", " fistulo")
         , V1=str_replace(.data$V1, " lipes", " filipes")
         , V1=str_replace(.data$V1, " mbriata", " fimbriata")
         , V1=str_replace(.data$V1, " liramum", " filiramum")
         , V1=str_replace(.data$V1, " lifolia", " filifolia")
         , V1=str_replace(.data$V1, " uitans", " fluitans")
         , V1=str_replace(.data$V1, "( a)([a-z]ellaris)", " fla\\2")
         , V1=str_replace(.data$V1, " exip", " flexip")
         , V1=str_replace(.data$V1, " cinale", "ficinale")
         , V1=str_replace(.data$V1, "Carex ssa", "Carex fissa"))%>% 
  filter(sl > 2)

# dat<-just_bluegrass
# old <- "bg"
# new <- "fixed"

conc <- function(dat, old, new){
  mutate(.data = dat, !!new := if_else(is.na(lag(.data[[old]]))
                                       , .data[[old]] # if true
                                       , if_else(grepl("^[[:blank:]]*-*[[:upper:]][[:lower:]]+ [[:lower:]]{3,}\\>", lag(.data[[old]]))
                                          , if_else(grepl("^[[:blank:]]*-*[[:upper:]][[:lower:]]+ [[:lower:]]{3,}\\>", .data[[old]])
                                                    , .data[[old]] # if innermost
                                                    , paste(lag(.data[[old]]), .data[[old]])) # if not innermost
                                          , .data[[old]])))
}

pasted1 <- knapp_next %>% conc(old = "V1", new = "paste1")
pasted2 <- pasted1 %>% conc("paste1", "paste2")
pasted3 <- pasted2 %>% conc("paste2", "paste3")
pasted4 <- pasted3 %>% conc("paste3", "paste4")
pasted5 <- pasted4 %>% conc("paste4", "paste5")
pasted6 <- pasted5 %>% conc("paste5", "paste6")
pasted7 <- pasted6 %>% conc("paste6", "paste7") 
# all(pasted7$paste6 == pasted7$paste7)
# sum(grepl("^[[:blank:]]*-*[[:upper:]]{1}[[:lower:]]+ [[:lower:]]+\\>",pasted7$paste7))
knapp_rows <- data.frame(obs_full = unique(pasted7$paste7))
                                       
towards_useful <- knapp_rows %>% 
  mutate( gs = str_extract(obs_full, "^-*[[:upper:]][[:lower:]]+ [[:lower:]]{3,}\\-*[[:lower:]]*")
          , verbiage = str_remove(obs_full, "^-*[[:upper:]][[:lower:]]+ [[:lower:]]{3,}\\-*[[:lower:]]*")
          , long_key = str_extract(obs_full, ".{30}")
          , exclude = grepl("†|waif | ‚Ä°|Excluded|‡", verbiage) & !grepl("Larix la", gs) 
          , verb_length = str_length(verbiage)) %>% 
  group_by(long_key) %>% 
  filter(verb_length == max(verb_length)) %>% 
  group_by(gs) %>% 
  mutate(has_nonNative_ssp = n_distinct(exclude)>1) %>% 
  separate(gs, into = c("genus", "species"), sep = " ", remove = FALSE) %>% 
  filter(genus != "Flora", genus != "Geographic") %>% 
  filter(species != "var") %>% 
  filter(species != "from") %>% 
  ungroup()

# checking regex work
# towards_useful %>% filter(grepl("fid", species))

n_distinct(towards_useful$gs)

# this still has about 250 more records than I think Wes Knapp's file did
# there should only be a few spp with 3-letter epithets

towards_useful %>% filter(str_length(species)<4)


# looks like those that exist are mostly character rendering problems.. now fixed 

# # Checks for character rendering issues past 

# towards_useful %>% filter(grepl("fluvia", obs_full))
# # check the fi and fl spp
# towards_useful[grepl("flave", towards_useful$species),]
# towards_useful[grepl("flex", towards_useful$species),]
# 
# towards_useful %>% filter(grepl("fluvia", obs_full)) %>% select(genus, species)
# towards_useful %>% filter(grepl("fidu", obs_full)) %>% select(species)
# towards_useful %>% filter(grepl("Viburnum", obs_full)) %>% select(gs)

# check out the duplicated spp
# View(towards_useful %>% group_by(gs) %>% mutate(dups = n()) %>% filter(dups >1))
# look fine now

# might have cleaned up all the truncated species epithets
towards_useful %>% filter(grepl("^[[:space:]]*[[:lower:]]{2,}", verbiage))

# make sure I got the invasives
towards_useful %>% filter(genus == "Cynodon")

n_distinct(towards_useful$gs) #3524



# View(towards_useful %>% filter(genus == "Cyperus") %>% select(species))



# Keep all sp with both native and non-native or excluded status as native,
# looks like they have non-native subsp or invalid observations

# Look at counts of native and excludes
towards_useful %>% group_by(exclude) %>% summarize(n_distinct(gs))
# 2018 good, 1457 bad (OMG, can't believe that I'm only catching this now)

# scan first thousand names of each group
# towards_useful %>% filter(exclude) %>% pull(gs) %>% unique()
# look at conservation status of excluded flora
# towards_useful %>% 
#   filter(exclude, grepl(" S[0-9]", verbiage)) %>% 
#   pull(obs_full)
# ok, Larix shouldn't be excluded. 
# Looks like the other spp/sspp Knapp and Naczi think aren't here


# towards_useful %>% filter(!exclude) %>% pull(gs) %>% unique()
# View(towards_useful)
# write.csv(towards_useful, "data/fromR/knapp_to_check.csv", row.names = FALSE)
# read.csv(towards_useful)

# check names against GBIF, I guess
# namecheck2 <- towards_useful %>%
#   mutate(gs = paste(genus, species)) %>%
#   pull(gs) %>%
#   sapply(function(x){
#     rgbif::name_backbone(name = x, verbose = TRUE)
#   }) %>%
#   map_dfr(bind_rows, .id = "gs")


namecheck <- towards_useful %>% 
  mutate(gs = paste(genus, species)) %>% 
  pull(gs) %>% 
  unique() %>% 
  sapply(function(x){
   nbRobust(x, kingdom = "Plantae" )
  }) %>% 
  map_dfr(bind_rows, .id = "gs")

# so it seems ok if there are multiple records if there is only one exact match 
# per taxon

# turns out that isn't the case. How about one species-level record?
# namecheck %>% 
#   filter(matchType == "EXACT") %>% 
#   group_by(gs, rank, status) %>% 
#   summarize(exactRanks =  n()) %>% 
#   arrange(desc(exactRanks))

# namecheck %>% 
#   filter(gs == "Elymus virginicus")
# this looks messy


# namecheck %>% 
#   group_by(gs) %>% 
#   summarize(sIDs = n_distinct(speciesKey)) %>% 
#   arrange(desc(sIDs))


# look at the confidence scores for name matching
namecheck %>%
  ggplot(aes(confidence, color = matchType))+
  geom_histogram() +
  theme_classic()

namecheck %>% filter(confidence >100)


# OK, looks like we never have more than one species ID number per gs? 
# This seesm good. 
# So why are there so many gs values? 
# I think it's multiple matches per knapp row, actually, which seems fine. 
namecheck %>% filter(matchType != "EXACT")
# ok, it looks like a decnt number of matches were to higher taxa, that's what I ned to focus on here. 
# namecheck %>% filter(rank != "SPECIES") %>% select(gs, rank)
# rgbif::name_backbone("Viburnum cassinoides", verbose = TRUE)

# cannonical names match gs 1:1
# namecheck %>% 
#   group_by(gs) %>% 
#   summarize(canon = n_distinct(canonicalName)) %>% 
#   arrange(desc(canon))

# namecheck <- namecheck %>% map_dfr(bind_rows, .id = "gs")

namecheck %>%
  mutate(total_gs = n_distinct(gs)) %>%  
  group_by(status) %>% 
  summarize(tibs = n()
            , spp = n_distinct(gs)
            , gss =mean(total_gs)) 

# whoa this is great, looks like most of the fuzzy matches are probably just gender things or misspellings and there isn't a ton to worry about here.
namecheck %>%
  mutate(total_gs = n_distinct(gs)) %>%  
  group_by(status, matchType) %>% 
  summarize(tibs = n()
            , spp = n_distinct(gs)
            , gss =mean(total_gs)) 

namecheck %>% filter(matchType == "FUZZY")

namecheck %>% filter(is.na(status)) %>% pull(gs)
# Heteranthera pauciflora is a good name not yet in backbone, it looks like (now solved!)
namecheck %>% filter(status == "SYNONYM") %>% pull(gs)

knapp_backboned <-
  towards_useful %>% 
  left_join(namecheck, by = "gs") %>% 
  rename(genus_knapp = genus.x
         , species_knapp = species.x
         , accepted_genus = genus.y
         , accepted_gs = species.y)


write.csv(knapp_backboned, "data/knapp_backboned.csv", row.names = FALSE)
knapp_backboned <- read.csv("data/knapp_backboned.csv")
knapp_backboned

                                        
