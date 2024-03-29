# code to fit RF models to occurrence data with covariates

library(tidyverse)
library(doParallel)
library(caret)
library(tictoc)




`%ni%` <- Negate(`%in%`) #convenience, this should be part of base R!
# custom summary functions
mu <- function(x){ifelse(is.numeric(x) & length(unique(x))>length(x)/8
                         , mean(x, na.rm =T)
                         , raster::modal(x, na.rm =T))}
sig <- function(x){ifelse(is.numeric(x) & length(unique(x))>length(x)/8
                          , sd(x, na.rm =T)
                          , length(unique(x)))}
# fitting function
source("code/RF_tuner.R")
# deal with categories
source("code/fix_mod.R")
# get the data
load(file="data/fromR/lfs/to_predict.RDA")
# unique(indi$roundedSRank)

# just some summary data:
# indi %>%
#   mutate(gs = paste(genus, species)) %>%
#   group_by(kingdomKey) %>%
#   summarize(occ = n(), spp = n_distinct(gs))




almost <- indi %>% dplyr::mutate(lat = sf::st_coordinates(.)[,1],
                                 lon = sf::st_coordinates(.)[,2]) %>% 
  group_by(genus, species, kingdomKey, simple_status) %>%
  mutate(maxlat = max(lat, na.rm =T)
         , minlat = min(lat, na.rm = T)
         , maxlon = max(lon, na.rm = T)
         , minlon = min(lon, na.rm = T)
         # , simple_status = factor(if_else(roundedSRank %in% c("S4","S5"), "secure"
                                          # , if_else(roundedSRank %in% c("S1", "S2", "S3", "SH"), "threatened", "NONE")
         # )
         )
  

almost %>% 
  ungroup() %>% 
  mutate(gs = paste(genus, species)) %>%
  group_by(kingdomKey, simple_status) %>%
  summarize(occ = n(), spp = n_distinct(gs))

tofit <-almost %>% 
  sf::st_drop_geometry() %>% 
  dplyr::select(-contains("exotic"))




# see what columns have issues of NA

# sapply(tofit_summary, function(x){sum(is.na(x))}) 
# looks good now... well, 155 not giving slope, maxlat, etc. 
# guessing those are singletons (very droppable)



no_sing <- tofit %>% 
  group_by(genus, species) %>% 
  mutate(nrec = n_distinct(lat)) %>% 
  filter(nrec > 1) %>% 
  dplyr::select(-c("nrec", "fcf")) # remove fcf because it creates NA

tofit_summary <- no_sing %>% 
  group_by(genus, species, kingdomKey) %>%
  summarize_all(.funs = c("mu", "sig")) %>% 
  mutate(Random_Pred = runif(1))

# drop na preemptively
tofit_summary_complete <- tofit_summary %>% drop_na()

# for the testing and training dataset, drop the ones with unknown status
classed <- tofit_summary_complete %>% 
  filter(simple_status_mu != "unranked") %>% 
  mutate(simple_status_mu = if_else(simple_status_mu =="threat", "secure", "threatened"))# 1 corresponds to "NONE"



classed.plant <- classed %>% filter(kingdomKey ==6) 
classed.lep <- classed %>% filter(kingdomKey ==1) 
# set random seed so results are same each time
set.seed(888)

# functions for fitting

folder <- function(dat, resp, k = 10, times = 10){
  createMultiFolds(dat[,resp][[1]], k = k, times = times)
}

make_status_mod <- function(dat = classed){
  as.formula(paste0("simple_status_mu ~ "
                    , paste(names(dat)[
                      !(grepl("status", names(dat))
                        | grepl("Rank", names(dat))
                        | grepl("genus", names(dat))
                        | grepl("species", names(dat))
                        | grepl("kingdom", names(dat))
                        | grepl("lat_sig", names(dat))
                        | grepl("lon_sig", names(dat))
                        | grepl("UID", names(dat))
                        | grepl("X2001_2019_change_index", names(dat))
                        | grepl("Red.1_mu", names(dat))
                      )
                      
                      # these variables will cause problems if they have
                      # cardinality 1, or they give away the answer
                    ]
                    , collapse= "+")
  ))
}

my_mod <- make_status_mod()









# maybe make life easier by dropping some info
dropper <- function(dat){dat[ , !(grepl("simple_status_sig", names(dat))
                                | grepl("Rank", names(dat))
                                | grepl("genus", names(dat))
                                | grepl("species", names(dat))
                                | grepl("kingdom", names(dat))
                                | grepl("UID", names(dat))
                                | grepl("X2001_2019_change_index", names(dat))
                                | grepl("Red.1_mu", names(dat))
                                )
                              ] 
  }

