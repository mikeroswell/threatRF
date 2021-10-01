# code to fit RF models to occurrence data with covariates

library(tidyverse)
library(randomForest)
`%ni%` <- Negate(`%in%`) #convenience, this should be part of base R!

load(file="data/fromR/to_predict.RDA")
unique(indi$roundedSRank)

tofit<-indi %>% dplyr::mutate(lat = sf::st_coordinates(.)[,1],
                              lon = sf::st_coordinates(.)[,2]) %>% 
  group_by(genus, species) %>%
  mutate(maxlat = max(lat, na.rm =T)
         , minlat = min(lat, na.rm = T)
         , maxlon = max(lon, na.rm = T)
         , minlon = min(lon, na.rm = T)
         , simple_status = factor(if_else(roundedSRank %in% c("S4","S5"), "secure"
                                   , if_else(roundedSRank %in% c("S1", "S2", "S3", "SH"), "threatened", "NONE")))) %>% 
  sf::st_drop_geometry()


resps<-unique(tofit$simple_status)


tofit_trimmed<-tofit %>% 
  ungroup %>% 
  select(-c(  ))


names(tofit)<-gsub("\\.", "A", names(tofit))

predictors<-  names(tofit)[names(tofit) %ni% c( "roundedSRank", "roundedNRank", "roundedGRank", "genus", "species", "lat", "lon", "simple_status", "geometry", names(tofit)[grepl("OBJECTID*", names(tofit))], names(tofit)[grepl("Descriptio*", names(tofit))] )]




tofit_complete<-tofit %>% drop_na(eval(predictors))
# tofit_complete %>% filter(simple_status %in% c("threatened", "secure")) %>% droplevels() %>% group_by(simple_status) %>% summarize(n())


first_RF_training <- randomForest(as.formula(paste0("simple_status ~ ", paste(predictors, collapse= "+")))
                           
                         , data = tofit_complete %>% filter(simple_status %in% c("threatened", "secure")) %>% droplevels()
                         # , ytest = c("threat", "secure")
                         , importance=TRUE
                         , na.action = na.exclude
                         , type = "classification"
)


first_RF_training
plot(first_RF_training)

pdf("figures/toy_model_variable_importance.pdf")
varImpPlot(first_RF_training)
dev.off()

first_RF_preds<- predict(first_RF_training, newdata = tofit_complete %>% filter(simple_status =="NONE"))
summary(first_RF_preds)
str(first_RF_preds)
with_preds<-bind_cols(tofit_complete %>% filter(simple_status =="NONE"), first_RF_preds)


# get rid of variables that work as species names
noname<-predictors[predictors %ni% c("minlat", "minlon", "maxlat", "maxlon")]
noname_RF_training <- randomForest(as.formula(paste0("simple_status ~ ", paste(noname, collapse= "+")))
                                 
                                 , data = tofit_complete %>% filter(simple_status %in% c("threatened", "secure")) %>% droplevels()
                                 # , ytest = c("threat", "secure")
                                 , importance=TRUE
                                 , na.action = na.exclude
                                 , type = "classification"
)

pdf("figures/variable_importance.pdf")
varImpPlot(noname_RF_training)
dev.off()

noname_RF_training
plot(noname_RF_training)

noname_RF_preds<- predict(noname_RF_training, newdata = tofit_complete %>% filter(simple_status =="NONE"))
summary(noname_RF_preds)
str(noname_RF_preds)
with_preds<-bind_cols(tofit_complete %>% filter(simple_status =="NONE"), noname_RF_preds)


prediction_rates = with_preds %>% group_by(genus, species) %>% summarize(records = n(), threatrate = sum(noname_RF_preds =="threatened")/n())
prediction_rates %>% ggplot(aes(records, threatrate))+
  scale_x_log10()+
  scale_y_log10()+
  geom_jitter()+
  theme_classic()

