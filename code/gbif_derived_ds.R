# code to create a derived gbif dataset
library(dplyr)
natives <- read.csv("data/fromR/lfs/native_records.csv")
head(natives) # doens't contain the dataset ids

all_gbif <- read.csv("data/fromR/lfs/occ_from_gbif.csv")

head(all_gbif)

dsKeys <- natives %>% 
  left_join(all_gbif, by = "gbifID") %>% 
  group_by(datasetKey) %>% 
  summarize(n())

head(dsKeys)

write.csv(dsKeys, "data/fromR/lfs/GBIF_derived_keys.csv", row.names = FALSE)
