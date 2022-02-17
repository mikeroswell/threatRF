# code to fit RF models to occurrence data with covariates

library(tidyverse)
library(doParallel)
library(caret)
library(pROC)
library(ROCR)
library(tictoc)


`%ni%` <- Negate(`%in%`) #convenience, this should be part of base R!
# custom summary functions
mu<-function(x){ifelse(is.numeric(x), mean(x, na.rm =T), raster::modal(x))}
sig<- function(x){ifelse(is.numeric(x), sd(x, na.rm =T), length(unique(x)))}

load(file="data/fromR/to_predict.RDA")
# unique(indi$roundedSRank)



tofit<-indi %>% dplyr::mutate(lat = sf::st_coordinates(.)[,1],
                              lon = sf::st_coordinates(.)[,2]) %>% 
  group_by(genus, species) %>%
  mutate(maxlat = max(lat, na.rm =T)
         , minlat = min(lat, na.rm = T)
         , maxlon = max(lon, na.rm = T)
         , minlon = min(lon, na.rm = T)
         , simple_status = factor(if_else(roundedSRank %in% c("S4","S5"), "secure"
                                          , if_else(roundedSRank %in% c("S1", "S2", "S3", "SH"), "threatened", "NONE")
                                                    )
                                          )
         ) %>% 
  filter(!exotic) %>% 
  sf::st_drop_geometry()


tofit_summary <-tofit%>% group_by(genus, species) %>%
  summarize_all(.funs = c("mu", "sig")) %>% 
    mutate(Random_Pred = runif(1))
# drop na preemptively
tofit_summary_complete<-tofit_summary %>% drop_na()

# for the testing and training dataset, drop the ones with unkown status
classed<-tofit_summary_complete %>% 
  filter(simple_status_mu != 1) %>% 
  mutate(simple_status_mu = if_else(simple_status_mu ==3, "secure", "threatened"))# 1 corresponds to "NONE"

# set random seed so results are same each time
set.seed(888)

n_obs<-nrow(classed)
# separate data into train and test
# test_rows<-sample(1:n_obs, round(0.2*n_obs))
# test<-classed[test_rows, ] # 60 rows
# train <- classed[-test_rows, ] # 240 rows
# 
# # see if threatened ("2") around twice rel. secure ("3") 
# train %>% group_by(simple_status_mu) %>% summarize(n()) # quite close to expected 2:1 in this run
# test %>% group_by(simple_status_mu) %>% summarize(n()) # good bit more even

# drop problematic variables

# subset a data.frame for categorical variables
get_categorical <- function(x) {
  x[sapply(x
                  , function(y) {
                    is.factor(y) | is.character(y)})]
}


# compare factor levels from two dataframes with identical variables
nomatch<-function(x, y){
  colnames(x)[!sapply(1:length(colnames(x)), function(x.name){
   all(y[ ,x.name] %in% x[ ,x.name])
  })]
}





my_mod<-as.formula(paste0("simple_status_mu ~ "
                  , paste(names(tofit_summary_complete)[
                    !(grepl("status", names(tofit_summary_complete))
                      | grepl("Rank", names(tofit_summary_complete))
                      # | names(tofit_summary_complete) %in% nomatch(get_categorical(train)
                                                                   # , get_categorical(test))
                      )
                    # these variables will cause problems
                        ]
                   , collapse= "+")
                  ))



outer_folds <- createMultiFolds(classed$simple_status_mu, k = 10, times =10)

source("code/RF_tuner.R")

fold_fits <- map( outer_folds, function(fold){
  tic()
  cl <- makePSOCKcluster(16)
  registerDoParallel(cl)
  
  rf = fit_rf(formu = my_mod
              , data = classed[fold, ]
              , sampling = NULL
              , tuneMethod = "repeatedcv"
              , repeats = 10
  )
  stopCluster(cl)
  print(toc())
  return(rf)
})

assess_method <- map_dfr(1:length(fold_fits), function(x){
  
  pre = predict(fold_fits[[x]]
                , classed[-outer_folds[[x]]]
                , type = "prob")
  predictions = prediction(pre[,2], as.factor(classed[-outer_folds[[x]], "simple_status_mu"]))
  preval = predict(fold_fits[[x]]
                   , classed[-outer_folds[[x]]]
  )
  in_auc = fold_fits[[x]]$results %>% 
    filter(mtry == fold_fits[[x]]$finalModel$mtry) %>% 
    pull(ROC)
  out_auc = performance(predictions, measure = "auc")@y.values[[1]] 
  #roc(response = example_train_dat[-y, 2], predictor = pre$Yes)
  data.frame(#pre
    #, truth = 
    #, 
    accuracy = sum(preval == example_train_dat[-outer_folds[[x]], 2])/length(y)
    , oob= mean(fold_fits[[x]]$finalModel$err.rate[, 1])
    , threat_acc = mean(fold_fits[[x]]$finalModel$err.rate[, 2])
    , sec_acc = mean(fold_fits[[x]]$finalModel$err.rate[, 3])
    , n_threat =  sum(classed[-outer_folds[[x]], "simple_status_mu"]=="threatened")
    , n_sec =  sum(classed[-outer_folds[[x]], "simple_status_mu"]=="secure")
    
    #, predictions = predictions@predictions
    , in_auc 
    , out_auc  # = as.numeric(my_auc$auc)
    , mod = x
    , mtry = fold_fits[[x]]$finalModel$mtry
  )
  
})




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
predict_unclassified<-predict(train_rf_orig, tofit_summary_complete %>% 
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

