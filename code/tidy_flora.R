# code to tidy untidy flora data
library(tidyverse)

# to create this file, saved .pdf of only the useful pages to "accessible text" 
# with acrobat
knapp_raw <- read.table("data/Knapp_word_to_Accessible_search_and_replace_emDash_special.txt", header = FALSE, sep = "\t")


knapp_raw
str(knapp_raw)


# learning data and regex

# grepl("^\\s[A-Z]{1}[a-z]* \\<[a-z]", "Sentence case")

# grepl("[[:blank:]]*^-*[[:upper:]][[:lower:]]* [[:lower:]]*\\>", c("-Yes yes", "Yes yes", "Yes yes yes", "Yes yes YES", "Almost almostAlmost", "no no", "NO no ", "nO", "No No")) # is almost a problem?

# str_extract(c("-Yes yes", "Yes yes", "Yes yes yes", "Yes yes YES", "Almost almostAlmost", "no no", "NO no ", "nO", "No No"), "^-*[[:upper:]][[:lower:]]+ [[:lower:]]{3,")

# grepl("^[[:blank:]]*-*[[:upper:]][[:lower:]]+ [[:lower:]]{3,}\\>", "Bluegrass, ‚Ä† (Kearny 70 US)")
# 
# str_extract(c("test", "your mom", "your mom is the best"), "^[a-z]*")
# add a useful sorting column, fix some of the issues from the "f" in the checklist
knapp_next <- knapp_raw %>% 
  mutate(sl = str_length(V1)
         , V1=str_replace(.data$V1, "( d)([a-z]{1,2}) ", "fid\\2 ")
         , V1=str_replace(.data$V1, "romanzof ", "romanzoffi")
         , V1=str_replace(.data$V1, "( or)([a-z]{1,2}) ", "flor\\2 ")
         , V1=str_replace(.data$V1, "ci ua", "ciflua")
         , V1=str_replace(.data$V1, "Wolf ella", "Wolffiella")
         , V1=str_replace(.data$V1, " ava ", " flava ")
         , V1=str_replace(.data$V1, "( )(ex[a-z]{1,2}) ", "f\\2 ")
         , V1=str_replace(.data$V1, "in rma ", "infirma ")
         , V1=str_replace(.data$V1, "of cinalis ", "officinalis ")
         , V1=str_replace(.data$V1, "proli cum", "prolificum")
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
  ungroup()

# checking regex work
# towards_useful %>% filter(grepl("fid", species))

n_distinct(towards_useful$gs)

# this still has about 250 more records than I think Wes Knapp's file did
# there should only be a few spp with 3-letter epithets

towards_useful %>% filter(str_length(species)<4)

# looks like those that exist are mostly character rendering problems.. now fixed 

# check out the duplicated spp
# View(towards_useful %>% group_by(gs) %>% mutate(dups = n()) %>% filter(dups >1))
# look fine now

# might have cleaned up all the truncated species epithets
towards_useful %>% filter(grepl("^[[:space:]]*[[:lower:]]{2,}", verbiage))

# make sure I got the invasives
towards_useful %>% filter(genus == "Cynodon")

unique(towards_useful$genus)

unique(towards_useful$gs)

n_distinct(towards_useful$gs)





# Keep all sp with both native and non-native or excluded status as native,
# looks like they have non-native subsp or invalid observations

# Look at counts of native and excludes
towards_useful %>% group_by(exclude) %>% summarize(n_distinct(gs))

# scan first thousand names of each group
towards_useful %>% filter(exclude) %>% pull(gs) %>% unique()
# look at conservation status of excluded flora
towards_useful %>% 
  filter(exclude, grepl(" S[0-9]", verbiage)) %>% 
  pull(obs_full)
# ok, Larix shouldn't be excluded. 
# Looks like the other spp/sspp Knapp and Naczi think aren't here


towards_useful %>% filter(!exclude) %>% pull(gs) %>% unique()
