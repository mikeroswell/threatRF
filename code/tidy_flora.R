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
knapp_next <- knapp_raw %>% 
  mutate(sl = str_length(V1)
         , V1=str_replace(.data$V1, "Passi ora", "Passiflora")
         , V1=str_replace(.data$V1, "tri dum", "trifidum")
         , V1=str_replace(.data$V1, "tri orum", "triflorum")) %>% 
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
all(pasted7$paste6 == pasted7$paste7)
sum(grepl("^[[:blank:]]*-*[[:upper:]]{1}[[:lower:]]+ [[:lower:]]+\\>",pasted7$paste7))
knapp_rows <- data.frame(obs_full = unique(pasted7$paste7))
                                       
towards_useful <- knapp_rows %>% 
  mutate( gs = str_extract(obs_full, "^-*[[:upper:]][[:lower:]]+ [[:lower:]]{3,}\\-*[[:lower:]]*")
          , verbiage = str_remove(obs_full, "^-*[[:upper:]][[:lower:]]+ [[:lower:]]{3,}\\-*[[:lower:]]*")
          , long_key = str_extract(obs_full, ".{30}")
          , exclude = grepl("†|waif | ‚Ä°|Excluded", verbiage) 
          , verb_length = str_length(verbiage)) %>% 
  group_by(long_key) %>% 
  filter(verb_length == max(verb_length)) %>% 
  group_by(gs) %>% 
  mutate(has_nonNative_ssp = n_distinct(exclude)>1) %>% 
  separate(gs, into = c("genus", "species"), sep = " ", remove = FALSE) %>% 
  filter(genus != "Flora")

# this still has about 250 more records than I think Wes Knapp's file did
# there should only be a few spp with 3-letter epithets

towards_useful %>% filter(str_length(species)<4)

# looks like those that exist are mostly character rendering problems


unique(towards_useful$genus)

unique(towards_useful$gs)

n_distinct(towards_useful$gs)

# Keep all sp with both native and non-native or excluded status as native,
# looks like they have non-native subsp or invalid observations


  arrange(desc(trouble))

  group_by(trouble) %>% 
  summarize(troubleSp = n())

  
towards_useful %>% filter(grepl("Poa pratensis", .data$obs_full))
towards_useful %>% filter(grepl("Passi", .data$obs_full))

towards_useful %>% filter(grepl("Galium tri", .data$obs_full))



towards_useful %>% filter(grepl("Bluegrass", obs_full))

knapp_raw %>% filter(grepl("Bluegrass", V1))
towards_useful %>% filter(grepl("Kearny 70 US", obs_full))

pasted3 %>% filter(grepl("Kearny 70 US", V1))

just_bluegrass <- data.frame(bg = knapp_raw$V1[7401:7402])

just_bluegrass
just_bluegrass %>% conc( "bg", "fixed")

paste(just_bluegrass$bg[1], just_bluegrass$bg[2])
# Poa pratensis should be fixed, see if it is an isolated incident

towards_useful %>% filter(grepl("Prunella vulgaris", .data$obs_full))
# looks like there is a native ssp

towards_useful %>% filter(grepl("Salsola kali", .data$obs_full))
# should use the comma to append next




                         

# I think 9th row is empty, what does it look like
summary(knapp_raw[9,])


# first drop empty rows
knapp_tight <- knapp_raw %>% filter(!(V1 == "" & V2 == "" & V3 == ""))

# loses >400 rows, good
knapp_tight$V1



names(knapp_raw) <- c("taxon", "common", "status", "note"  )

# figure out how to filter data properly
# grepl("ACEA", knapp_raw$taxon)
# is.na(knapp_raw$common)
# grepl("^$", knapp_raw$co



# grepl("ACEA", knapp_raw$taxon) & grepl("^$", knapp_raw$common)
# is_null(knapp_raw$common)

# remove extra spaces
knapp_ss <- mutate_all(knapp_raw, function(x){ gsub("  ", " ", x)})

knapp_typed <- knapp_ss %>% mutate(varType = if_else(grepl("ACEA", .data$taxon) & grepl("^$",.data$common), "family" 
                     , if_else(!grepl("ACEA", .data$taxon) & grepl("^$",.data$common) & !grepl(" ", .data$taxon) | grepl("FLOWERING PLANTS", .data$taxon), "phylum"
                               , "species"))) %>% 
  mutate(taxon_native = gsub("\\*", "exotic_", taxon))

knapp_flat <- knapp_typed %>% 
  pivot_wider(names_from = varType , values_from = taxon_native, values_fn = "first") %>% 
  fill(phylum) %>% 
  fill(family) %>% 
  filter(!is.na(species)) %>% 
  select(-taxon) %>% 
  mutate(native = !grepl("exotic_", species)
         , species = gsub("exotic_", "", species)
         , family = str_to_sentence(family)) %>% 
  separate(species, into = c("genus", "species", "subsp_or_var", "additional_tax", "additional_tax_2", "additional_tax_3", "additional_tax_4"), sep = " ")

# check that all waifs are not native
knapp_flat %>% group_by(status, native) %>% summarize(n()) %>% arrange(native)

#looks like lots of spelling/capitalization errors likely

# View(knapp_flat %>% filter(!native & status == "S2"))

# these are both native, see if I can figure out what's up by looking back 
# before I manipulated the data

# looks like there is an error!

write.csv(knapp_flat, file = "data/fromR/flat_flora.csv", row.names = FALSE)
View(knapp_flat[seq(1, 3100, 150),])
