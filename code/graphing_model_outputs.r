# source("code/RF_setup.R")
library(pROC)
library(ROCR)
library(tidyverse)
library(patchwork)

# set bigger font for poster
theme_set(theme_classic(base_size = 24))

load("data/fromR/lfs/100_100_fits_20221021_espindolab.rda")

load("data/fromR/outerFolds.RDA")
classed.test <- read.csv("data/fromR/training_data.csv")
classed.lep.test <-classed.test %>% filter(kingdomKey == 1)
classed.plant.test <- classed.test %>% filter(kingdomKey == 6)
all.equal(str(classed.plant), str(classed.plant.test))
all.equal(classed.plant$lat_sig, classed.plant.test$lat_sig)
is.wholenumber <-
  function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol

# need to get the type of certain variables straight
str(classed.lep.test)

classed.lep <- classed.lep.test %>% 
  mutate_all(.funs =function(x){
  if(is.integer(x)){
    if(all(is.wholenumber(x))){as.factor(x)}
  }
  else{x}
})


classed.plant <-  classed.plant.test %>% 
  mutate_all(.funs = function(x){
  if(is.integer(x)){
    if(all(is.wholenumber(x))){as.factor(x)}
  }
  else{x}
}) 



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
    remod = fix.mod(mod = mod, test.dat = out.dat, resp = resp)
    
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
  fits = trees_leps[[1]][[2]]
  , subdat = dropper(classed.plant)
  , folds = trees_leps[[1]][[3]]
  
)

plant_auc_hist <- plant_assess %>% 
  ggplot(aes(out_auc))+ 
  geom_histogram()+
  geom_vline(xintercept = mean(plant_assess$out_auc), color = "red") +
  geom_vline(xintercept = 0.5, color = "black", linetype = 5) +
  labs(x = "AUROC on unseen data in 10x repeated \n10-fold cross validation"
       , y = "frequency") +
  annotate("text", x = 0.2, y = 12, label = "plantae", size = 9) +
  scale_x_continuous(limits = c(0, 1.05), breaks = seq(0, 1, 0.25))


  # scan for relatioships between accuracy, AUC, and mtry
plant_assess %>% 
  ggplot(aes(mtry, out_auc, color = accuracy)) + 
  geom_point()
  # theme_classic()


# great for looking at some stuff but I don't think this one is necessary to keep around
# pdf("figures/accuracy_and_CVP_subset_data.pdf")
# plant_assess %>% 
#   ggplot(aes(in_auc, out_auc, color = accuracy)) + 
#   geom_point()+
#   theme_classic() + 
#   xlim(0,1)+
#   ylim(0,1)
# dev.off()
# look at distribution of auc
# hist(plant_assess$out_auc)

lep_assess<-assess_method(
  fits = trees_leps[[2]][[2]]
  , subdat = dropper(classed.lep)
  , folds = trees_leps[[2]][[3]]
  
)

lep_auc_hist <- lep_assess %>% 
  ggplot(aes(out_auc))+ 
  geom_histogram()+
  geom_vline(xintercept = mean(lep_assess$out_auc), color = "red") +
  geom_vline(xintercept = 0.5, color = "black", linetype = 5) +
  labs(x = ""
       , y = "frequency") +
  annotate("text", x = 0.2, y = 46, label = "lepidoptera", size = 9) +
  xlim(c(0,1))


pdf("figures/model_performance_histograms_subdata.pdf")
lep_auc_hist / plant_auc_hist
dev.off()

summary(lm(out_auc~mtry, data = lep_assess))
sum_success(lep_assess)
sum_success(plant_assess)
# # scan for relatioships between accuracy, AUC, and mtry
# lep_assess %>% 
#   ggplot(aes(mtry, out_auc, color = accuracy)) + 
#   geom_point()+
#   theme_classic()

# # inauc vs outauc
# lep_assess %>% 
#   ggplot(aes(in_auc, out_auc, color = accuracy)) + 
#   geom_point()+
#   theme_classic() + 
#   xlim(0,1)+
#   ylim(0,1)





# look at distribution of auc
# hist(lep_assess$out_auc)

# check out variable importances

# str(trees_leps[[tax]][[2]][[x]]$finalModel$importance)

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
# next step is to create better 
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


save(final_fits, file = "data/fromR/lfs/final_fits_20221031_espindolab.RDA")
load("data/fromR/lfs/final_fits_20221031_espindolab.RDA")

# get "optimal" thresholds
threshlist<-map(1:2, function(tax){
  kk <- c(1,6)[tax]
  raw <- classed %>%
    filter(kingdomKey == kk)
    
  dat <- dropper(raw)
  preds <- predict(object = fix.mod(final_fits[[tax]]
                                      , dat
                                      , "simple_status_mu")
                     , newdata = dat
                     , type = "prob")
  perf = performance(prediction(preds[2], as.numeric(as.factor(dat$simple_status_mu))), "sens", "spec")

  thresh.df <- data.frame(cut = perf@alpha.values[[1]], sens = perf@x.values[[1]], spec = perf@y.values[[1]])
  thresh <- thresh.df[which.max(thresh.df$sens + thresh.df$spec), ]
  taxon <- c("lepidoptera", "plantae")[tax]

   return(list(taxon, all_thresh = data.frame(taxon, thresh.df), best_thresh = data.frame(taxon, thresh),
               train.preds = data.frame(preds, taxon, genus = raw$genus, species = raw$species)))
})

save(threshlist, file = "data/fromR/threshlist.rda")
load("data/fromR/threshlist.rda")
# 
# 
# # optimal thresholds
ot<-map_dfr(1:2, function(tax){
  threshlist[[tax]]$best_thresh
})

threshlist
ot

# make predictions on the new data
predict_unclassified <- map_dfr(c(1, 2), function(tax){
  kk <- c(1,6)[tax]
  raw <- tofit_summary %>% 
    filter(kingdomKey == kk & simple_status_mu == 1 ) %>% 
    drop_na()
  dat <- dropper(raw)
  og <- fix.mod(final_fits[[tax]]
                , dat
                , "simple_status_mu")
  preds <- predict(object = fix.mod(final_fits[[tax]]
                                    , dat
                                    , "simple_status_mu")
                   , newdata = dat
                   , type="prob")
  
  bind_cols(raw %>% select(genus, species)
            , preds
            , taxon = c( "lepidoptera", "plantae" )[tax])
})

predict_preclassified<-map_dfr(1:2, function(tax){
  threshlist[[tax]]$train.preds
})

# combine predictions with original data
w_preds <- almost %>% 
  ungroup() %>% 
  left_join(bind_rows(predict_unclassified, predict_preclassified)
            , by = c("genus", "species")) 

w_preds %>% 
  group_by(taxon, genus, species) %>% 
  summarize(threat_prob = mean(threatened)) %>% 
  ggplot(aes(threat_prob, fill = taxon, color = taxon)) +
  geom_histogram() +
  theme_classic()




# plot predictions at the observation level
pdf("figures/probability_threatened_subdata.pdf", width = 10, height = 10)
w_preds %>% 
  filter(!is.na(taxon)) %>% 
  mutate(existing = c("no prior status", "known threatened", "known rel. secure")[as.numeric(as.factor(simple_status))]) %>% 
  ggplot(aes(color = threatened))+
  geom_sf(size = 0.1) +
  scale_color_viridis_c() +
  theme_classic(base_size = 20) +
  theme(legend.position = "bottom"
        , panel.spacing = unit(1.5, "lines")
        , legend.text = element_text(angle = 90, hjust = 1)
        , axis.text.x = element_text(angle = 90)
        ) +
  facet_grid(existing ~ taxon) + 
  guides(guide_legend(title.hjust = 1, title.vjust = 0)) +
  labs(color = "predicted probability   \nspecies is threatened   ")
dev.off()

# rasterize predictions

# first make a raster that includes the state of MD
base_rast <- raster::raster(w_preds, resolution = 8000, crs = sf::st_crs(w_preds) ) # hopefully means 8 km


# threshold for saying something is probably threatened
# over_thresh<-function(x, ...){
#   if_else(sum(na.omit(x))>0
#           , sum(na.omit(x) > threat_thres)/sum(na.omit(x) > 0)
#           , 0)}


pred_by_spp_70 <- w_preds %>%
  group_by(genus, species, taxon) %>%
  mutate(is.threatened = #c("pSecure", "pThreatened")[2-
           as.numeric(threatened > 0.7)# ot$cut[as.numeric(as.factor(taxon))])
         #]
         , gs = paste(genus, species, sep = "_"))

pred_by_spp_thresh <- w_preds %>%
  group_by(genus, species, taxon) %>%
  mutate(is.threatened = #c("pSecure", "pThreatened")[2-
           as.numeric(threatened > ot$cut[as.numeric(as.factor(taxon))])
         #]
         , gs = paste(genus, species, sep = "_")) 
# %>%
# %>%
#   pivot_wider(names_from = is.threatened, values_from = gs, id_cols = NULL )

plusOne <- function(x){length(unique(x))}

species_threatened_per_cell <- map(c("lepidoptera", "plantae"), function(tax){
  pred_by_spp_thresh %>%
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

existe <- function(x){as.numeric(length(x)>0)}

backMap <- pred_by_spp_thresh %>% 
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
            , aes(x = x, y = y, fill = unadd(layer))) +
  coord_equal() +
  theme_void() +
  scale_fill_viridis_c(na.value = "white") +
  labs(title = "Number of lepidopteran species predicted \nto be threatened at optimal \ncutoff (>59.6% of votes) per cell"
       , fill = "species")

ggplot()+
  geom_tile(data = as.data.frame(as(raster::overlay(species_threatened_per_cell[[2]]
                                                    , backMap
                                                    , fun = function(x){sum(x, na.rm =TRUE)}),  "SpatialPixelsDataFrame"))
            , aes(x = x, y = y, fill = unadd(layer))) +
  coord_equal() +
  theme_void() +
  scale_fill_viridis_c(na.value = "white") +
  labs(title = "Number of plant species predicted \nto be threatened at optimal \ncutoff (>64.2% of votes) per cell"
       , fill = "species")
dev.off()

species_not_threatened_per_cell <- map(c("lepidoptera", "plantae"), function(tax){
  pred_by_spp_thresh %>%
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
  pred_by_spp_thresh %>%
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
                  , fun = function(x,y){(x+0.0001)/y})
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
      , field = "threat"
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
    theme_void(base_size = 20) +
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
  sf::st_drop_geometry() %>% 
  filter(roundedSRank %in% c("S1", "S2", "S3", "S4", "S5")) %>% 
  ggplot(aes(as.numeric(as.factor(roundedSRank)), threatened))+
  ggbeeswarm::geom_beeswarm()+
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



