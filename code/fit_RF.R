# code to fit RF models to occurrence data with covariates

library(tidyverse)
library(doParallel)
library(furrr)
library(caret)
library(pROC)
library(ROCR)
library(tictoc)
library(patchwork)


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

almost <- indi %>% dplyr::mutate(lat = sf::st_coordinates(.)[,1],
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
  )

tofit <-almost %>% 
  sf::st_drop_geometry() %>% 
  select(-contains("exotic"))


tofit_summary <- tofit%>% group_by(genus, species, kingdomKey) %>%
  # mutate(ab = n()) %>% 
  summarize_all(.funs = c("mu", "sig")) %>% 
    mutate(Random_Pred = runif(1))

# see what columns have issues of NA

# sapply(tofit_summary, function(x){sum(is.na(x))}) 
# looks good now... well, 155 not giving slope, maxlat, etc. 
# guessing those are singletons (very droppable)


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

save(trees_leps, file="data/fromR/lfs/100_100_fits_20220727.rda")

load("data/fromR/lfs/100_100_fits_20220530.rda")
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

plant_auc_hist <- plant_assess %>% 
  ggplot(aes(out_auc))+ 
  geom_histogram()+
  geom_vline(xintercept = mean(plant_assess$out_auc), color = "red") +
  geom_vline(xintercept = 0.5, color = "black", linetype = 5) +
  labs(x = "AUROC on unseen data in 10x repeated 10-fold cross validation"
       , y = "frequency") +
  geom_text(aes(x = 0.1, y = 12), label = "plantae",) +
  xlim(c(0,1))+
  theme_classic()



# scan for relatioships between accuracy, AUC, and mtry
plant_assess %>% 
  ggplot(aes(mtry, out_auc, color = accuracy)) + 
  geom_point()+
  theme_classic()



pdf("figures/accuracy_and_CVP_subset_data.pdf")
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

lep_auc_hist <- lep_assess %>% 
  ggplot(aes(out_auc))+ 
  geom_histogram()+
  geom_vline(xintercept = mean(lep_assess$out_auc), color = "red") +
  geom_vline(xintercept = 0.5, color = "black", linetype = 5) +
  labs(x = ""
       , y = "frequency") +
  geom_text(x = 0.1, y = 46, label = "lepidoptera") +
  xlim(c(0,1))+
  theme_classic()


pdf("figures/model_performance_historgrams_subdata.pdf")
lep_auc_hist / plant_auc_hist
dev.off()

summary(lm(out_auc~mtry, data = lep_assess))
sum_success(lep_assess)
sum_success(plant_assess)
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

str(trees_leps[[tax]][[2]][[x]]$finalModel$importance)

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
  summarize(meanRank = mean(rnk)
            , upprRank = quantile(rnk, 0.975)
            , lowrRank = quantile(rnk, 0.025)) %>% 
  ungroup() %>% 
  group_by(taxon) %>% 
  mutate(taxMin = min_rank(meanRank)) %>% 
  right_join(varimp_run) 


#make variable importance plot
pdf("figures/variable_importance_top.pdf")
vimp_sum %>% 
  group_by(varName) %>% 
  mutate(mincomTax = min(taxMin), minMeanRank = min(meanRank)) %>%
  filter(mincomTax <6) %>% 
  ungroup() %>%
  arrange(minMeanRank) %>%
  mutate(varName = as.factor(varName) %>% fct_reorder(desc(minMeanRank))) %>%
  mutate(taxon = if_else(taxon == "plant", "plantae", "lepidoptera")) %>% 
  ggplot(aes(varName, rnk, fill = taxon, color = taxon))+
  geom_violin(width = 3, position =position_dodge(width = 0.4), alpha = 0.8) +  
  coord_flip() + 
  guides(color = "none") +
  labs(y = "variable importance rank (low is best)", x = "") +
  scale_fill_manual(values = hcl.colors(4, palette = "Batlow")[c(3,2)]) +
  scale_color_manual(values = hcl.colors(4, palette = "Batlow")[c(3,2)]) +
  theme_classic() 
dev.off()  



# 
# # more variable importance stuff copied from another file, maybe useful
# varimp_run<-map_dfr(1:length(trees_leps[[2]][[2]]), function(x){
#   data.frame(data.frame(trees_leps[[2]][[2]][[x]]$finalModel$importance) %>%
#                arrange(desc(MeanDecreaseAccuracy)) %>% 
#                rownames_to_column() %>%  
#                mutate(rnk = row_number()) %>% 
#                select(rnk, MeanDecreaseAccuracy, varName = rowname)
#              , foldrep = x)
# })
# 
# varimp_run2<-map_dfr(1:length(trees_leps[[1]][[2]]), function(x){
#   data.frame(data.frame(trees_leps[[1]][[2]][[x]]$finalModel$importance) %>%
#                arrange(desc(MeanDecreaseAccuracy)) %>% 
#                rownames_to_column() %>%  
#                mutate(rnk = row_number()) %>% 
#                select(rnk, MeanDecreaseAccuracy, varName = rowname), foldrep = x)
# })
# 
# varimp_run2 %>% group_by(varName) %>% summarize(meanRank = mean(rnk)) %>% arrange(meanRank)
# 
# head(vimp_sum, 20)

# fit final models
n_cores <- 16
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
load("data/fromR/lfs/final_fits.RDA")
# get "optimal" thresholds
# threshlist<-map(1:2, function(tax){
#   kk <- c(1,6)[tax]
#   raw <- tofit_summary_complete %>% 
#     filter(kingdomKey == kk, simple_status_mu != 1)
#   dat <- dropper(raw)
#   preds <- predict(object = fix.mod(final_fits[[tax]]
#                                       , dat
#                                       , simple_status_mu)
#                      , newdata = dat
#                      , type = "prob")
#   perf = performance(prediction(preds[1], dat$simple_status_mu-2), "sens", "spec")
# 
#   thresh.df <- data.frame(cut = perf@alpha.values[[1]], sens = perf@x.values[[1]], spec = perf@y.values[[1]])
#   thresh <- thresh.df[which.max(thresh.df$sens + thresh.df$spec), ]
#   taxon <- c("lepidoptera", "plantae")[tax]
# 
#    return(list(taxon, all_thresh = data.frame(taxon, thresh.df), best_thresh = data.frame(taxon, thresh), 
#                train.preds = data.frame(preds, taxon, genus = raw$genus, species = raw$species)))
# })
# 
# save(threshlist, file = "data/fromR/threshlist.rda")
load("data/fromR/threshlist.rda")
 

# optimal thresholds
ot<-map_dfr(1:2, function(tax){
  threshlist[[tax]]$best_thresh
})

threshlist
ot

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

predict_preclassified<-map_dfr(1:2, function(tax){
  threshlist[[tax]]$train.preds
})
  
# combine predictions with original data
w_preds <- almost %>% ungroup() %>% left_join(bind_rows(predict_unclassified, predict_preclassified), by = c("genus", "species")) 

w_preds %>% 
  group_by(taxon, genus, species) %>% 
  summarize(threat_prob = mean(threatened)) %>% 
  ggplot(aes(threat_prob, fill = taxon, color = taxon)) +
  geom_histogram() +
  theme_classic()




# plot predictions at the observation level
pdf("figures/probability_threatened_subdata.pdf")
w_preds %>% 
  filter(!is.na(taxon)) %>% 
  mutate(existing = c("no prior status", "known threatened", "known relatively secure")[as.numeric(simple_status)]) %>% 
  ggplot(aes(color = threatened))+
  geom_sf(size = 0.1) +
  scale_color_viridis_c() +
  theme_classic() +
  theme(legend.position = "bottom") +
  facet_grid(existing ~ taxon) + 
  labs(color = "predicted probability \nspecies is threatened")
dev.off()

# rasterize predictions

# first make a raster that includes the state of MD
base_rast <- raster::raster(w_preds, resolution = 8000, crs = sf::st_crs(w_preds) ) # hopefully means 8 km


 # threshold for saying something is probably threatened
# over_thresh<-function(x, ...){
#   if_else(sum(na.omit(x))>0
#           , sum(na.omit(x) > threat_thres)/sum(na.omit(x) > 0)
#           , 0)}


pred_by_spp<-w_preds %>%
  group_by(genus, species, taxon) %>%
  mutate(is.threatened = #c("pSecure", "pThreatened")[2-
           as.numeric(threatened > ot$cut[as.numeric(as.factor(taxon))])
         #]
             , gs = paste(genus, species, sep = "_")) 
# %>%
#   pivot_wider(names_from = is.threatened, values_from = gs, id_cols = NULL )

plusOne<-function(x){length(unique(x))}

species_threatened_per_cell <- map(c("lepidoptera", "plantae"), function(tax){
  pred_by_spp %>%
    filter(taxon == tax, simple_status =="NONE", is.threatened == 1) %>%
    raster::rasterize(
      y = base_rast
      , fun = function(x, ...){
        c(
        plusOne(x)
        )}
      , field = "gs"
      , na.rm = FALSE)
})

existe<-function(x){as.numeric(length(x)>0)}
backMap<-pred_by_spp %>% 
  raster::rasterize(
  y = base_rast
  , fun = function(x, ...){
    c(existe(x)
    )}
    , field = "gs"
    , na.rm = TRUE)

unadd<- function(x){
  x<- x-1
  x[x<0]<-NA
  return(x)
}

pdf(file = "figures/n_spp_cell.pdf")
ggplot()+
  geom_tile(data = as.data.frame(as(raster::overlay(species_threatened_per_cell[[1]]
                     , backMap
                     , fun = function(x){sum(x, na.rm =TRUE)}),  "SpatialPixelsDataFrame"))
            , aes(x = x, y = y, fill = factor(unadd(layer)))) +
  coord_equal() +
  theme_void() +
  scale_fill_viridis_d() +
  labs(title = "Number of lepidopteran species predicted threatened per cell"
       , fill = "species")

ggplot()+
  geom_tile(data = as.data.frame(as(raster::overlay(species_threatened_per_cell[[2]]
                                                    , backMap
                                                    , fun = function(x){sum(x, na.rm =TRUE)}),  "SpatialPixelsDataFrame"))
            , aes(x = x, y = y, fill = unadd(layer))) +
  coord_equal() +
  theme_void() +
  scale_fill_viridis_c(na.value = "white") +
  labs(title = "Number of plant species predicted threatened per cell"
       , fill = "species")
dev.off()

species_not_threatened_per_cell <- map(c("lepidoptera", "plantae"), function(tax){
  pred_by_spp %>%
    filter(taxon == tax, simple_status =="NONE", is.threatened == 0) %>%
    raster::rasterize(
      y = base_rast
      , fun = function(x, ...){
        c(
          n_distinct(x)
        )}
      , field = "gs"
      , na.rm = FALSE)
})


species_per_cell <- map(c("lepidoptera", "plantae"), function(tax){
  pred_by_spp %>%
    filter(taxon == tax, simple_status =="NONE") %>%
    raster::rasterize(
      y = base_rast
      , fun = function(x, ...){
        c(
          n_distinct(x)
        )}
      , field = "gs"
      , na.rm = TRUE)
})

p_spp_threatened <-map(1:2, function(tax){
  raster::overlay(species_threatened_per_cell[[tax]], species_per_cell[[tax]]
                                   , fun = function(x,y){x+0.0001/y})
})




n_over_thresh<-function(x, ...){sum(na.omit(x>threat_thres))}
mean_sd_pred <- map(c("lepidoptera", "plantae"), function(tax){
  w_preds %>%
  filter(taxon == tax, simple_status =="NONE") %>% 
  raster::rasterize(
    y = base_rast
    , fun =function(x, ...){c(
      mean(x)
      , sd(x))}
    , field = "threatened"
    , na.rm = FALSE)
})

pro<-function(x){sum(x)/length(x)}

count_occ_threatened <- map(c("lepidoptera", "plantae"), function(tax){
  w_preds %>%
    filter(taxon == tax, simple_status =="NONE") %>% 
    mutate(overThresh = as.numeric(threatened > ot$cut[2-as.numeric(tax == "lepidoptera")])) %>% 
    raster::rasterize(
      y = base_rast
      , fun =function(x, ...){c(
        sum(x)
        , pro(x)
        )}
      , field = "overThresh"
      , na.rm = TRUE)
})

count_occ_threatened[[1]]

pdf("figures/threat_prob_predictions.pdf")

map(1:2, function(tax){
  map(1:2, function(x){
    ggplot()+
    geom_tile(data = as.data.frame(as(mean_sd_pred[[tax]][[x]]
                                      , "SpatialPixelsDataFrame"))
              , aes(x = x, y = y, fill = .data[[paste0("layer.", x)]]
                    )
              )+
    scale_fill_viridis_c() +
    coord_equal() +
    theme_void() +
    labs(title = c("lepidoptera", "plantae")[tax]
         , fill = c(
      "mean of predicted threat \nprobability across all \noccurrences in cell"
      , "standard deviation of \npredicted threat probability \nacross occurrences in cell" )[x]
    )
  })
})

dev.off()

pdf("figures/occurrence_predictions.pdf")

map(1:2, function(tax){
  map(1:2, function(x){
    ggplot()+
      geom_tile(data = as.data.frame(as(count_occ_threatened[[tax]][[x]]
                                        , "SpatialPixelsDataFrame"))
                , aes(x = x, y = y, fill = .data[[paste0("layer.", x)]]
                )
      )+
      scale_fill_viridis_c() +
      coord_equal() +
      theme_void() +
      labs(title = c("lepidoptera", "plantae")[tax]
           , fill = c(
             "occurrences of species predicted to be threatened"
             , "proportion of occcurrences predicted to be threatened species")[x]
      )
  })
})

dev.off()

pdf("figures/prop_species_threatened_rasterized.pdf")
map(1:2, function(tax){
    ggplot()+
      geom_tile(data = as.data.frame(as(p_spp_threatened[[tax]], "SpatialPixelsDataFrame"))
                , aes(x = x, y = y, fill = .data[["layer"]]
                )
      )+
      scale_fill_viridis_c() +
      coord_equal() +
      theme_void() +
      labs(title = c("lepidoptera", "plantae")[tax]
           , fill = c(
             
             "proportion observations \nwith predicted probability \nof being threatened \n>70% " )
      )
  })
dev.off()

pdf("figures/n_species_threatened_rasterized.pdf")
map(1:2, function(tax){
  ggplot()+
    geom_tile(data = as.data.frame(as(species_threatened_per_cell[[tax]], "SpatialPixelsDataFrame"))
              , aes(x = x, y = y, fill = .data[["layer"]]
              )
    )+
    scale_fill_viridis_c() +
    coord_equal() +
    theme_void() +
    labs(title = c("lepidoptera", "plantae")[tax]
         , fill = c(
           
           "species \nwith predicted probability \nof being threatened \n>70% " )
    )
})
dev.off()

pdf("figures/spp_per_cell.pdf")
map(1:2, function(tax){
  ggplot()+
    geom_tile(data = as.data.frame(as(species_per_cell[[tax]], "SpatialPixelsDataFrame"))
              , aes(x = x, y = y, fill = .data[["layer"]]
              )
    )+
    scale_fill_viridis_c() +
    coord_equal() +
    theme_void() +
    labs(title = c("lepidoptera", "plantae")[tax]
         , fill = c(
           
           "species \nwith predicted probability \nof being threatened \n>70% " )
    )
})
dev.off()

w_preds %>%
  sf::st_drop_geometry() %>% 
  filter(simple_status == "NONE") %>% 
  group_by(genus, species, taxon) %>% 
  summarize(threat_pred = mean(threatened)) %>% 
  arrange(desc(threat_pred))

# ordinal stuff

w_preds %>% 
  filter(roundedSRank %in% c("S1", "S2", "S3", "S4", "S5")) %>% 
  ggplot(aes(as.numeric(as.factor(roundedSRank)), threatened))+
  geom_point()+
  geom_smooth()+
  theme_classic()+
  facet_wrap(~kingdomKey)

# look at the threatened species
View(predict_unclassified %>% 
  arrange(desc(threatened)) %>% 
    filter(taxon == "lepidoptera")
  )

View(predict_unclassified %>% 
       arrange(desc(threatened)) %>% 
       filter(taxon == "plantae")
)


  
