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



# set number of workers for cluster

cores<-16
co<-0

# fit models
future::plan(strategy = "multiprocess", workers = cores)

trees_leps<-map(c("lep", "plant"), function(tax){
 
  main <- get(paste0("classed.", tax ))
  classy <- dropper(main)
  outer_folds <- folder(classy, "simple_status_mu")
  save(outer_folds, file = "data/fromR/outerFolds.RDA")
  # fit models
  fold_fits <- furrr::future_map(1:length(outer_folds), function(fold){ # 
    tic()
    co<-co+1

    rf <- fit_rf(formu = my_mod
                , data = classy[outer_folds[[fold]], ]
                , sampling = NULL
                , tuneMethod = "repeatedcv"
                , repeats = 10
    )


    print(toc())
   
    return(rf)
    
  })
  save(fold_fits, file = "data/fromR/fold_fits.RDA")
  print("did the fits")
  # m_assess <- assess_method()
  # print("assess method ran")
  # m_sum <- sum_success(m_assess)
  # print(co)
  # print(exists(fold_fits))
  return(list(tax, fold_fits, outer_folds))
})

save(trees_leps, file="data/fromR/lfs/100_100_fits_20220422.rda")

load("data/fromR/lfs/100_100_fits_20220422.rda")
# get performance
assess_method <- function(fits = NULL
                          , subdat = NULL 
                          , fulldat = NULL
                          , folds = NULL
                          , resp = "simple_status_mu"
                          , pos = "threatened"
                          , neg = "secure"){
  map_dfr(1:length(fits), function(x){
    out.dat = subdat[-folds[[x]], ]
    mod = fits[[x]]
    remod = fix.mod(mod, out.dat, resp = resp)
    pre = predict(remod
                  , out.dat 
                  , type = "prob")
    predictions = prediction(pre[,2], subdat[-folds[[x]], resp])
    preval = predict(remod, out.dat)
    in_auc = remod$results %>% 
      filter(mtry == remod$finalModel$mtry) %>% 
      pull(ROC)
    out_auc = performance(predictions, measure = "auc")@y.values[[1]] 
    data.frame(
      accuracy = sum(preval == subdat[-folds[[x]],] %>% pull(resp))/length(preval)
      , oob_accuracy = 1- mean(remod$finalModel$err.rate[, 1])
      , threat_acc = 1- mean(remod$finalModel$err.rate[, 2])
      , sec_acc = 1- mean(remod$finalModel$err.rate[, 3])
      , n_threat =  sum(subdat[-folds[[x]], resp]== pos)
      , n_sec =  sum(subdat[-folds[[x]], resp]== neg)
      , in_auc 
      , out_auc  # = as.numeric(my_auc$auc)
      , mod = x
      , mtry =fits[[x]]$finalModel$mtry
    )
  })    
}



sum_success <- function(m_assess){
  m_assess %>% summarize(across(.fns =list(mean = mean, sd = sd)))
}

plant_assess<-assess_method(
  fits = trees_leps[[2]][[2]]
  , subdat = dropper(classed.plant)
  , folds = trees_leps[[2]][[3]]
  
)

hist(plant_assess$mtry)


# scan for relatioships between accuracy, AUC, and mtry
plant_assess %>% 
  ggplot(aes(mtry, out_auc, color = accuracy)) + 
  geom_point()+
  theme_classic()


summary(lm(out_auc~mtry, data = lep_assess))

pdf("figures/accuracy_and_CV.pdf")
plant_assess %>% 
  ggplot(aes(in_auc, out_auc, color = accuracy)) + 
  geom_point()+
  theme_classic() + 
  xlim(0,1)+
  ylim(0,1)
dev.off()
# look at distribution of auc
hist(plant_assess$out_auc)

lep_assess<-assess_method(
  fits = trees_leps[[1]][[2]]
  , subdat = dropper(classed.plant)
  , folds = trees_leps[[1]][[3]]
  
)

# scan for relatioships between accuracy, AUC, and mtry
lep_assess %>% 
  ggplot(aes(mtry, out_auc, color = accuracy)) + 
  geom_point()+
  theme_classic()

# inauc vs outauc
lep_assess %>% 
  ggplot(aes(in_auc, out_auc, color = accuracy)) + 
  geom_point()+
  theme_classic() + 
  xlim(0,1)+
  ylim(0,1)





# look at distribution of auc
hist(lep_assess$out_auc)

# check out variable importances

varimp_run<-map_dfr(1:2, function(tax){
  map_dfr(1:length(trees_leps[[tax]][[2]]), function(x){
    data.frame(data.frame(trees_leps[[tax]][[2]][[x]]$finalModel$importance) %>%
               arrange(desc(MeanDecreaseAccuracy)) %>% 
               rownames_to_column() %>%  
               mutate(rnk = row_number(), foldrep = x,  taxon = c("lep", "plant")[tax]) %>% 
               select(rnk, MeanDecreaseAccuracy, varName = rowname, foldrep, taxon))
  })
})

vimp_sum<-varimp_run %>% 
  group_by(varName, taxon) %>% 
  summarize(meanRank = mean(rnk)) %>% 
  arrange(meanRank)

head(vimp_sum, 20)

# fit final models
n_cores <- 7 
final_fits <- map(c("lep", "plant"), function(tax){
  main <- get(paste0("classed.", tax ))
  classy <- dropper(main)
  cl <- makePSOCKcluster(n_cores)
  registerDoParallel(cl)
    # fit models
  rf <- fit_rf(formu = my_mod
                 , data = classy
                 , sampling = NULL
                 , tuneMethod = "repeatedcv"
                 , repeats = 10
    )
  stopCluster(cl)
    return(rf)
    
  })

save(final_fits, file = "data/fromR/lfs/final_fits.RDA")

# get "optimal" thresholds
threshlist<-map(1:2, function(tax){
  kk <- c(1,6)[tax]
  raw <- tofit_summary_complete %>% 
    filter(kingdomKey == kk, simple_status_mu != 1)
  dat <- dropper(raw)
  preds <- predict(object = fix.mod(final_fits[[tax]]
                                      , dat
                                      , simple_status_mu)
                     , newdata = dat
                     , type = "prob")
  perf = performance(prediction(preds[1], dat$simple_status_mu-2), "sens", "spec")

  thresh.df <- data.frame(cut = perf@alpha.values[[1]], sens = perf@x.values[[1]], spec = perf@y.values[[1]])
  thresh <- thresh.df[which.max(thresh.df$sens + thresh.df$spec), ]
  taxon <- c("lepidoptera", "plantae")[tax]

   return(list(taxon, all_thresh = data.frame(taxon, thresh.df), best_thresh = data.frame(taxon, thresh)))
})

save(threshlist, file = "data/fromR/threshlist.rda")

 

# make predictions on the new data
predict_unclassified <- map_dfr(c(1, 2), function(tax){
  kk <- c(1,6)[tax]
  raw <- tofit_summary_complete %>% 
    filter(kingdomKey == kk, simple_status_mu == 1)
  dat <- dropper(raw)
  preds <- predict(object = fix.mod(final_fits[[tax]]
                                    , dat
                                    , simple_status_mu)
                   , newdata = dat
                   , type="prob")
  
  bind_cols(raw %>% select(genus, species)
            , preds
            , taxon = c("lepidoptera", "plantae")[tax])
})
  
# combine predictions with original data
w_preds <- tofit %>% left_join(predict_unclassified, by = c("genus", "species"))

w_preds <- w_preds %>% mutate(simple_status = factor(if_else(roundedSRank %in% c("S4","S5"), "secure"
                                           , if_else(roundedSRank %in% c("S1", "S2", "S3", "SH"), "threatened", "NONE")))
                   , p.threatened = if_else(is.na(threatened), as.numeric(simple_status =="threatened"), threatened)
                   , had_status = if_else(is.na(threatened), "had_status", "predicted")
                   )


# plot predictions at the observation level
pdf("figures/probability_threatened.pdf")
w_preds %>% 
  ggplot(aes(color = p.threatened))+
  geom_sf(size = 0.2) +
  scale_color_viridis_c() +
  theme_classic() +
  theme(legend.position = "bottom") +
  facet_wrap(~had_status, nrow= 2)
dev.off()

# rasterize predictions

# first make a raster that includes the state of MD
base_rast <- raster::raster(w_preds, resolution = 5000, crs = sf::st_crs(w_preds) ) # hopefully means 5 km


threat_thres<-0.9 # threshold for saying something is probably threatened
over_thresh<-function(x, ...){
  if_else(sum(na.omit(x))>0
          , sum(na.omit(x) > threat_thres)/sum(na.omit(x) > 0)
          , 0)}
rast_pred <- w_preds %>%
  filter(had_status =="predicted") %>% 
  raster::rasterize(
    y = base_rast
    , fun =function(x, ...){c(
      mean(x)
      , over_thresh(x)    
      , sd(x))}
    , field = "p.threatened"
    , na.rm = FALSE)


pdf("figures/example_raster_with_proportion_probably_threatened.pdf")
x<-1
map(1:3, function(x){
  
  ggplot()+
    geom_tile(data = as.data.frame(as(rast_pred[[x]], "SpatialPixelsDataFrame"))
              , aes(x = x, y = y, fill = .data[[paste0("layer.", x)]]
                    )
              )+
    scale_fill_viridis_c() +
    coord_equal() +
    theme_void() +
    labs(fill = c(
      "mean of predicted threat \nprobability across all \noccurrences in cell"
      , "proportion observations \nwith predicted probability \nof being threatened \n>90% "
      , "standard deviation of \npredicted threat probability \nacross occurrences in cell" )[x]
    )
})

dev.off()

w_preds %>%
  sf::st_drop_geometry() %>% 
  filter(had_status == "predicted") %>% 
  group_by(genus, species) %>% 
  summarize(threat_pred = mean(p.threatened)) %>% 
  arrange(desc(threat_pred))

