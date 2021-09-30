# code to fit RF models to occurrence data with covariates

library(tidyverse)
library(randomForest)

load(file="data/fromR/to_predict.RDA")


tofit<-indi %>% group_by(genus, species) %>%
  mutate(maxlat = max(latitude, na.rm =T)
         , minlat = min(latitude, na.rm = T)
         , maxlon = max(longitude, na.rm = T)
         , minlon = min(longitude, na.rm = T)
         , simple_status = as.factor(simple_status))

resps<-unique(tofit$simple_status)

predictors<-  c("LU_CODE"
, "bioclim1"
, "bioclim2"
, "bioclim3"
, "bioclim4"
, "bioclim5"
, "bioclim6"
, "bioclim7"
, "bioclim8"
, "maxlat"
, "minlat"
, "maxlon"
, "minlon"
)
tofit_complete<-tofit %>%  drop_na(eval(predictors))

first_RF_training <- randomForest(simple_status ~
                           LU_CODE
                         + bioclim1
                         + bioclim2
                         + bioclim3
                         + bioclim4
                         + bioclim5
                         + bioclim6
                         + bioclim7
                         + bioclim8
                         + maxlat
                         + minlat
                         + maxlon
                         + minlon
                         , data = tofit_complete %>% filter(simple_status %in% c("threat", "secure")) %>% droplevels()
                         # , ytest = c("threat", "secure")
                         , importance=TRUE
                         , na.action = na.exclude
)

pdf("figures/toy_model_variable_importance.pdf")
varImpPlot(first_RF_training)
dev.off()

first_RF_preds<- predict(first_RF_training, data = tofit_complete %>% filter(simple_status =="unranked"))
summary(first_RF_preds)
str(first_RF_preds)
with_preds<-bind_cols(tofit_complete %>% filter(simple_status =="unranked"), first_RF_preds)
prediction_rates = with_preds %>% group_by(genus, species) %>% summarize(records = n(), threatrate = sum(first_RF_preds =="threat")/n())

