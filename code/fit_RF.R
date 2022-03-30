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
load(file="data/fromR/to_predict.RDA")
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
  mutate(ab = n()) %>% 
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

folder <- function(dat, resp, k = 10, times = 5){
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
    predictions = prediction(pre[,2], get(fulldat)[-get(folds)[[x]], resp])
    preval = predict(remod, out.dat)
    in_auc = remod$results %>% 
      filter(mtry == remod$finalModel$mtry) %>% 
      pull(ROC)
    out_auc = performance(predictions, measure = "auc")@y.values[[1]] 
    #roc(response = example_train_dat[-y, 2], predictor = pre$Yes)
    data.frame(
      accuracy = sum(preval == get(fulldat)[-get(folds)[[x]],] %>% pull(get(resp)))/length(preval)
      , oob_accuracy = 1- mean(remod$finalModel$err.rate[, 1])
      , threat_acc = 1- mean(remod$finalModel$err.rate[, 2])
      , sec_acc = 1- mean(remod$finalModel$err.rate[, 3])
      , n_threat =  sum(get(fulldat)[-get(folds)[[x]], resp]== pos)
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

trees_leps<-map(c("lep", "plant"), function(tax){
  main <- get(paste0("classed.", tax ))
  classy <- dropper(main)
  outer_folds <- folder(classy, "simple_status_mu")
  # fit models
  fold_fits <- map(outer_folds, function(fold){
    tic()
    cl <- makePSOCKcluster(8)
    registerDoParallel(cl)
    
    rf = fit_rf(formu = my_mod
                , data = classy[fold, ]
                , sampling = NULL
                , tuneMethod = "repeatedcv"
                , repeats = 5
    )
    stopCluster(cl)
    print(toc())
    return(rf)
  })
  
  m_assess <- assess_method()
  m_sum <- sum_success(m_assess)
  
  return(list(tax, fold_fits, m_assess, m_sum))
})

trees_leps[[2]][[4]]

trees_leps[[2]][[2]]
pdf('figures/model_stability_question.pdf')
map_dfr(1:50, function(f){
  mod<-trees_leps[[2]][[2]][[f]]
  data.frame(
    f, 
    mtry= mod$finalModel$mtry
  )
  
}) %>%  ggplot(aes(mtry))+geom_histogram() +
  theme_classic()+
  labs(y = "folds with mtry selected") +
  geom_vline(xintercept = 0.5)

dev.off()

trees_leps[[2]][[3]] %>% ggplot(aes(mtry, out_auc))+
  geom_point()+
  theme_classic()+
  geom_hline(yintercept = 0.5, color = "red")

trees_leps[[2]][[3]] %>% ggplot(aes(mtry, in_auc))+
  geom_point()+
  theme_classic()+
  geom_hline(yintercept = 0.5, color = "red")

trees_leps[[2]][[3]] %>% ggplot(aes(mtry, accuracy))+
  geom_point()+
  theme_classic()+
  geom_hline(yintercept = 0.5, color = "red")


pdf("figures/auc_inner_vs_outer.pdf")
trees_leps[[2]][[3]] %>% ggplot(aes(in_auc, out_auc, color = oob_accuracy))+
  geom_point()+
  theme_classic()+
  ylim(c(0,1))+
  xlim(c(0,1))
dev.off()
  
geom_hline(yintercept = 0.5, color = "red")

trees_leps[[2]][[3]] %>% ggplot(aes(oob_accuracy, out_auc))+
  geom_point()+
  theme_classic()+
  
  geom_hline(yintercept = 0.5, color = "red")

trees_leps[[2]][[3]] %>% ggplot(aes(oob_accuracy, accuracy))+
  geom_point()+
  theme_classic()+
  geom_hline(yintercept = 0.5, color = "red")

pdf("figures/auc_plant_data.pdf")
trees_leps[[2]][[3]] %>% ggplot(aes(out_auc))+geom_histogram()+theme_classic()
dev.off()

pdf("figures/oob_accuracy_plant_data.pdf")
trees_leps[[2]][[3]] %>% ggplot(aes(oob_accuracy))+geom_histogram()+theme_classic()
dev.off()
summary(lm(out_auc~oob_accuracy, data = trees_leps[[2]][[3]]))
summarize(sum(mtry<6)/n()) 


save(trees_leps, file ="data/fromR/trees_leps_mods.rda")

# tictoc::tic()
# cl <- makePSOCKcluster(7)
# registerDoParallel(cl)
# 
# #models using training data, assessed with 10-fold cv
# up_train <-  fit_rf(data = train, formu = my_mod, sampling="up", tuneMethod = "repeatedcv")
# down_train <- fit_rf(data = train
#                            , my_mod, sampling = "down", tuneMethod = "repeatedcv")
# orig_train <- fit_rf(train
#                            , my_mod, tuneMethod = "repeatedcv")
# 
# # models using training data, with tuning, 10-fold cv
# up_tune <- fit_rf(train 
#                               , my_mod, sampling="up", tuneMethod = "repeatedcv", mtry =2:25 )
# down_tune <- fit_rf(train
#                                 , my_mod, sampling = "down", tuneMethod = "repeatedcv", mtry =2:25)
# orig_tune <- fit_rf(train 
#                                 , my_mod, tuneMethod = "repeatedcv", mtry =2:25)
# 
# 
# #models using full dataset, no tuning
# up_fulldata <- fit_rf(train 
#                          , my_mod, sampling="up", tuneMethod = "LOOCV")
# down_fulldata <- fit_rf(train 
#                            , my_mod, sampling = "down", tuneMethod = "LOOCV")
# orig_fulldata <- fit_rf(train 
#                            , my_mod, tuneMethod = "LOOCV")
# stopCluster(cl)
# tictoc::toc()
# 
# auc_comp <- map_dfr(c("up", "down", "orig"), function(sampling){
#   map_dfr(c("train", "tune", "fulldata"), function(data_provided){
#     mod_setup = paste(sampling, data_provided, sep = "_")
#  data.frame(mod_setup
#             , get(eval(mod_setup))$results %>% 
#               filter(mtry == get(eval(mod_setup))$finalModel$mtry )
#           )
#   })
# }) %>% 
#   arrange(-ROC)
# 
# 
# auc_comp<-map_dfr(c("up", "down", "orig"), function(sampling){
#   map_dfr(c("train", "tune", "fulldata"), function(data_provided){
#     mod_setup = paste(sampling, data_provided, sep = "_")
#     data.frame(mod_setup
#                , get(eval(mod_setup))$results %>% 
#                  filter(mtry == get(eval(mod_setup))$finalModel$mtry)
#                 , oob= mean(get(eval(mod_setup))$finalModel$err.rate[,1])
#                 , secure= mean(get(eval(mod_setup))$finalModel$err.rate[,2])
#                 , threatened = mean(get(eval(mod_setup))$finalModel$err.rate[,3])
#     )
#   })
# })
# 
# auc_comp

map(c("up", "down", "orig"), function(sampling){
  map(c("train", "tune", "fulldata"), function(data_provided){
    print(paste(sampling, data_provided, sep = "_"))
   print( get(paste(sampling, data_provided, sep = "_"))$finalModel$ )
    
  })
})

write.csv(auc_comp, "data/fromR/auc_comparisons.csv", row.names =F) 

#what would a stable mmodel look like?

sd(rbinom(50, 27, 0.72)/50)

# save(train_rf_down, file = "data/fromR/lfs/rf_down_plants.rda")
# save(train_rf_up, file = "data/fromR/lfs/rf_up_plants.rda")
# save(train_rf_orig, file = "data/fromR/lfs/rf_base_plants.rda")


# see if including more data massively increases kappa
tictoc::tic()
train_rf_up<- fit_rf(train, my_mod, sampling="up")
train_rf_down<- fit_rf(train, my_mod, sampling = "down")

train_rf_orig<-fit_rf(train, my_mod)
tictoc::toc()

# test that thing!
train_rf_up$finalModel
test_rf_discrete<-predict(train_rf_up, test) #13% OOB
sum(test$simple_status_mu == test_rf_discrete)/length(test_rf_discrete) # 61% accuracy on test

test %>% 
  bind_cols(is.correct = test$simple_status_mu ==test_rf_discrete) %>% 
  group_by(simple_status_mu) %>% 
  summarize(n=n(), correct = sum(is.correct), prop.correct = sum(is.correct)/n())

train_rf_down$finalModel
test_rf_discrete_d<-predict(train_rf_down, test) # 36% oob
sum(test$simple_status_mu == test_rf_discrete_d)/length(test_rf_discrete_d) # 75% accuracy on test
test %>% 
  bind_cols(is.correct = test$simple_status_mu ==test_rf_discrete_d) %>% 
  group_by(simple_status_mu) %>% 
  summarize(n=n(), correct = sum(is.correct), prop.correct = sum(is.correct)/n())

train_rf_orig$finalModel
test_rf_discrete_o<-predict(train_rf_orig, test)
sum(test$simple_status_mu == test_rf_discrete_o)/length(test_rf_discrete_o) # 62%

test %>% 
  bind_cols(is.correct = test$simple_status_mu ==test_rf_discrete_o) %>% 
  group_by(simple_status_mu) %>% 
  summarize(n=n(), correct = sum(is.correct), prop.correct = sum(is.correct)/n())




# slightly better than expected from the tuning
confusion<-test %>% ungroup() %>% mutate(prediction=test_rf_discrete
                                         , orig=test_rf_discrete_o
                                         , down = test_rf_discrete_d)
confusion %>%
  mutate(gotit_up = case_when(simple_status_mu == prediction~1, TRUE ~ 0 )
         , gotit_orig = case_when(simple_status_mu == orig~1, TRUE  ~ 0 )
         , gotit_down = case_when(simple_status_mu == down~1, TRUE  ~ 0 )
         ) %>%  
  group_by(simple_status_mu) %>% 
  summarize( e_rate_up = 1-sum(gotit_up)/n()
            , e_rate_orig = 1-sum(gotit_orig)/n()
            ,  e_rate_down = 1-sum(gotit_down)/n()
            , tot = n())
# but isn't much better in test. Good to go with it for now? Maybe. 


# refit with all data? If so, need to make sure hyperparameters are saved etc.,
# otherwise this can be a very different model
# final_rf<-fit_rf(classed, my_mod)

# plot(varImp(final_rf))
# variable importance plots!
pdf("figures/plant_importance.pdf")
varImpPlot(train_rf_orig$finalModel)
varImpPlot(train_rf_up$finalModel)
varImpPlot(train_rf_down$finalModel)
dev.off()

# make predictions on the new data
predict_unclassified<-predict(train_rf_orig, tofit_summary %>% 
                                filter(simple_status_mu ==1)
                                       , type="prob")




prob_preds<-tofit_summary_complete %>% 
  filter(simple_status_mu ==1) %>% 
  select(genus, species) %>% 
  bind_cols(predict_unclassified) %>% 
  rename("rel_secure"="2", "threatened" = "3")


w_preds <- indi %>% left_join(prob_preds, by = c("genus", "species"))

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

