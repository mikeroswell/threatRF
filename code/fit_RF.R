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


# tofit %>% mutate(gs = paste(genus, species, sep = "_")) %>% group_by(simple_status) %>% summarize(genera = n_distinct(genus), spp = n_distinct(gs))




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
data.frame(first_RF_training$importance) %>% arrange(desc(MeanDecreaseGini))
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


# use species names, not min/max lat/lon
withname<- names(tofit)[names(tofit) %ni% c( "roundedSRank", "roundedNRank", "roundedGRank", "genus", "lat", "lon", "simple_status", "geometry", names(tofit)[grepl("OBJECTID*", names(tofit))], names(tofit)[grepl("Descriptio*", names(tofit))], "minlat", "minlon", "maxlat", "maxlon")]
withname_RF_training <- randomForest(as.formula(paste0("simple_status ~ ", paste(withname, collapse= "+")))
                                   
                                   , data = tofit_complete %>% filter(simple_status %in% c("threatened", "secure")) %>% droplevels()
                                   # , ytest = c("threat", "secure")
                                   , importance=TRUE
                                   , na.action = na.exclude
                                   , type = "classification"
)
varImpPlot(withname_RF_training)
withname_RF_training


names_and<- names(tofit)[names(tofit) %ni% c( "roundedSRank", "roundedNRank", "roundedGRank", "genus", "lat", "lon", "simple_status", "geometry", names(tofit)[grepl("OBJECTID*", names(tofit))], names(tofit)[grepl("Descriptio*", names(tofit))])]

namesand_RF_training <- randomForest(as.formula(paste0("simple_status ~ ", paste(names_and, collapse= "+")))
                                     
                                     , data = tofit_complete %>% filter(simple_status %in% c("threatened", "secure")) %>% droplevels()
                                     # , ytest = c("threat", "secure")
                                     , importance=TRUE
                                     , na.action = na.exclude
                                     , type = "classification"
)

varImpPlot(namesand_RF_training)
namesand_RF_training

pdf("figures/variable_importance.pdf")
varImpPlot(noname_RF_training)
dev.off()




phigh<-predict(first_RF_training, newdata = tofit_complete)
plow<-predict(noname_RF_training, newdata = tofit_complete)
pname<-predict(withname_RF_training, newdata = tofit_complete)

all_pred<-tofit_complete %>% bind_cols(phigh_stat = phigh, plow_stat = plow)

get_a_picture <- function(x, prediction){
  print(prediction)
  x %>% 
    group_by(genus, species, simple_status) %>% 
    summarize(agree = sum(as.character(get(prediction)) == as.character(simple_status))/n()
              , frequency_threatened = sum(get(prediction) =="threatened")/n()
              , n_obs= n())
}

picture<-function(df){df %>% ggplot(aes(simple_status, frequency_threatened, color = log(n_obs)))+
  ggbeeswarm::geom_quasirandom( dodge.width = 0.9, alpha = 0.9, size = 1.2)+
  theme_classic() +
  scale_color_viridis_c()}

pdf("figures/threat_prediction_frequency.pdf")
picture(get_a_picture("phigh_stat"))
dev.off()

# make some maps

resf<-sf::st_as_sf(all_pred
                   , coords = c("lat", "lon")
                   , crs = "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")

fancy_sf<-resf %>%  group_by(genus, species, simple_status) %>%   
  mutate(agree = sum(as.character(phigh_stat) == as.character(simple_status))/n()
            , frequency_threatened = sum(phigh_stat =="threatened")/n()
            , n_obs= n()
         , best_guess = if_else(simple_status =="NONE", phigh_stat, simple_status))

fancy_sf %>% ggplot(aes(color = best_guess))+
  geom_sf(size = 1, alpha =0.8) +
  theme_classic() +
  scale_color_viridis_d()
