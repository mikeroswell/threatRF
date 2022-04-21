# code to fit RF models to occurrence data with covariates

library(tidyverse)
library(doParallel)
library(caret)
library(pROC)
library(ROCR)
library(tictoc)


`%ni%` <- Negate(`%in%`) #convenience, this should be part of base R!
# custom summary functions
mu <- function(x){ifelse(is.numeric(x), mean(x, na.rm =T), raster::modal(x, na.rm =T))}
sig <- function(x){ifelse(is.numeric(x), sd(x, na.rm =T), length(unique(x)))}
# fitting function
source("code/RF_tuner.R")
# deal with categories
source("code/fix_mod.R")
# get the data
load(file="data/fromR/lfs/to_predict.RDA")
# unique(indi$roundedSRank)



tofit<-indi %>% dplyr::mutate(lat = sf::st_coordinates(.)[,1],
                              lon = sf::st_coordinates(.)[,2]) %>% 
  group_by(genus, species, kingdomKey) %>%
  mutate(maxlat = max(lat, na.rm =T)
         , minlat = min(lat, na.rm = T)
         , maxlon = max(lon, na.rm = T)
         , minlon = min(lon, na.rm = T)
         , simple_status = factor(if_else(roundedSRank %in% c("S4","S5"), "secure"
                                          , if_else(roundedSRank %in% c("S1", "S2", "S3", "SH"), "threatened", "NONE")
                                                    )
                                          )
         ) %>% 
  # filter(!exotic) %>%  # need to add this back in. 
  sf::st_drop_geometry()


tofit_summary <- tofit%>% group_by(genus, species, kingdomKey) %>%
  # mutate(ab = n()) %>% 
  summarize_all(.funs = c("mu", "sig")) %>% 
    mutate(Random_Pred = runif(1))

# see what columns have issues of NA


  


# drop na preemptively
tofit_summary_complete<-tofit_summary %>% drop_na()

# for the testing and training dataset, drop the ones with unknown status
classed<-tofit_summary_complete %>% 
  filter(simple_status_mu != 1) %>% 
  mutate(simple_status_mu = if_else(simple_status_mu ==3, "secure", "threatened"))# 1 corresponds to "NONE"

classed.plant <- classed %>% filter(kingdomKey ==6) 
classed.lep <- classed %>% filter(kingdomKey ==1) 
# set random seed so results are same each time
set.seed(888)

# functions for fitting

folder <- function(dat, resp, k = 10, times = 10){
  createMultiFolds(dat[,resp][[1]], k = k, times = times)
}

make_status_mod<-function(dat = classed){
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
                       )
                    
                    # these variables will cause problems if they have
                    # cardinality 1, or they give away the answer
                        ]
                   , collapse= "+")
                  ))
}

my_mod <- make_status_mod()




  
  



# maybe make life easier by dropping some info
dropper<-function(dat){dat[ , !(grepl("simple_status_sig", names(dat))
                                               | grepl("Rank", names(dat))
                                               | grepl("genus", names(dat))
                                               | grepl("species", names(dat))
                                               | grepl("kingdom", names(dat))
                                               | grepl("UID", names(dat))
                                               | grepl("X2001_2019_change_index", names(dat))
)
                                             ] 
}

# get performance
assess_method <- function(fits = "fold_fits"
                          , subdat = "classy" 
                          , fulldat = "main"
                          , folds = "outer_folds"
                          , resp = "simple_status_mu"
                          , pos = "threatened"
                          , neg = "secure"){
  map_dfr(1:length(get(fits)), function(x){
    out.dat = get(subdat)[-get(folds)[[x]], ]
    mod = get(fits)[[x]]
    remod = fix.mod(mod, out.dat, resp = resp)
    pre = predict(remod
                  , out.dat 
                  , type = "prob")
    predictions = prediction(pre[,2], get(subdat)[-get(folds)[[x]], resp])
    preval = predict(remod, out.dat)
    in_auc = remod$results %>% 
      filter(mtry == remod$finalModel$mtry) %>% 
      pull(ROC)
    out_auc = performance(predictions, measure = "auc")@y.values[[1]] 
    #roc(response = example_train_dat[-y, 2], predictor = pre$Yes)
    data.frame(
      accuracy = sum(preval == get(subdat)[-get(folds)[[x]],] %>% pull(get(resp)))/length(preval)
      , oob_accuracy = 1- mean(remod$finalModel$err.rate[, 1])
      , threat_acc = 1- mean(remod$finalModel$err.rate[, 2])
      , sec_acc = 1- mean(remod$finalModel$err.rate[, 3])
      , n_threat =  sum(get(subdat)[-get(folds)[[x]], resp]== pos)
      , n_sec =  sum(get(fulldat)[-get(folds)[[x]], resp]== neg)
      , in_auc 
      , out_auc  # = as.numeric(my_auc$auc)
      , mod = x
      , mtry = get(fits)[[x]]$finalModel$mtry
    )
  })    
}



sum_success <- function(m_assess){
  m_assess %>% summarize(across(.fns =list(mean = mean, sd = sd)))
}

# set number of workers for cluster

cores<-50

# fit models
future::plan(strategy = "multiprocess", workers = cores)

trees_leps<-map(c("lep", "plant"), function(tax){
 
  main <- get(paste0("classed.", tax ))
  classy <- dropper(main)
  outer_folds <- folder(classy, "simple_status_mu")
  # fit models
  fold_fits <- furrr::future_map(1:length(outer_folds), function(fold){
    tic()


    rf = fit_rf(formu = my_mod
                , data = classy[outer_folds[[fold]], ]
                , sampling = NULL
                , tuneMethod = "repeatedcv"
                , repeats = 10
    )


    print(toc())
    return(rf)
  })
    return(list(tax, fold_fits, outer_folds))
})

save(trees_leps, file = "data/fromR/lfs/trees_leps_amarel.rda")
