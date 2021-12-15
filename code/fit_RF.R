# code to fit RF models to occurrence data with covariates

library(tidyverse)
library(randomForest)
`%ni%` <- Negate(`%in%`) #convenience, this should be part of base R!

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
                                   , if_else(roundedSRank %in% c("S1", "S2", "S3", "SH"), "threatened", "NONE")))) %>% 
  filter(!exotic) %>% 
  sf::st_drop_geometry()
mu<-function(x){ifelse(is.numeric(x), mean(x, na.rm =T), raster::modal(x))}
sig<- function(x){ifelse(is.numeric(x), sd(x, na.rm =T), length(unique(x)))}
tofit_summary <-tofit%>% group_by(genus, species) %>%
  summarize_all(.funs = c("mu", "sig")) %>% 
    mutate(Random_Pred = runif(1))

# tofit %>% mutate(gs = paste(genus, species, sep = "_")) %>% group_by(simple_status) %>% summarize(genera = n_distinct(genus), spp = n_distinct(gs))





# names(tofit)<-gsub("\\.", "A", names(tofit))

# names(tofit_summary)<-gsub("\\.", "A", names(tofit_summary))

predictors<-  names(tofit)[names(tofit) %ni% c( "roundedSRank", "roundedNRank", "roundedGRank", "genus", "species", "exotic", "lat", "lon", "simple_status", "geometry", names(tofit)[grepl("OBJECTID*", names(tofit))], names(tofit)[grepl("Descriptio*", names(tofit))] )]




# tofit_complete<-tofit %>% drop_na(eval(predictors)) 

tofit_summary_commplete<-tofit_summary %>% drop_na()
# tofit_complete %>% filter(simple_status %in% c("threatened", "secure")) %>% droplevels() %>% group_by(simple_status) %>% summarize(n())


# first_RF_training <- randomForest(as.formula(paste0("simple_status ~ ", paste(predictors, collapse= "+")))
#                            
#                          , data = tofit_complete %>% filter(simple_status %in% c("threatened", "secure")) %>% droplevels()
#                          # , ytest = c("threat", "secure")
#                          , importance = TRUE
#                          , na.action = na.exclude
#                          , type = "classification"
# )
# 
summarized_RF_training <- randomForest(as.formula(paste0("as.factor(simple_status_mu) ~ "
                                                         , paste(names(tofit_summary_commplete)[
                                                           !(grepl("status", names(tofit_summary_commplete))
                                                             |grepl("Rank", names(tofit_summary_commplete)))]
                                                           , collapse= "+")))
                                  
                                  , data = tofit_summary_commplete 
                                  %>% filter(simple_status_mu %in% c(2,3)) 
                                  %>% droplevels()
                                  # , ytest = c("threat", "secure")
                                  , importance = TRUE
                                  , na.action = na.exclude
                                  , type = "classification"
)

summarized_RF_training
varImpPlot(summarized_RF_training)


plot(summarized_RF_training)
data.frame(summarized_RF_training$importance) %>% arrange(desc(MeanDecreaseAccuracy))

data.frame(first_RF_training$importanceSD) %>% arrange(desc(MeanDecreaseAccuracy))

summarized_RF_training$importanceSD

pdf("figures/var_importance.pdf")
varImpPlot(first_RF_training, n.var = 7, main = "variable importance vs. random predictor")
dev.off()


sd(tofit_complete$bioclim7)
sd(tofit_complete$bioclim5)



data.frame(importance(first_RF_training)) %>% arrange(desc(MeanDecreaseAccuracy))
first_RF_training$importance/first_RF_training$importanceSD


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


presum<-predict(summarized_RF_training, newdata = tofit_summary_commplete)
simple_pred<-tofit_summary_commplete %>% bind_cols(presum)
predictions_from_summary<-tofit %>% left_join(tofit_summary_commplete %>% bind_cols(presum=presum))

phigh<-predict(first_RF_training, newdata = tofit_complete)
plow<-predict(noname_RF_training, newdata = tofit_complete)
pname<-predict(withname_RF_training, newdata = tofit_complete)

all_pred<-tofit_complete %>% bind_cols(phigh_stat = phigh) # , plow_stat = plow)

get_a_picture <- function(x, prediction){
  print(prediction)
  x %>% 
    group_by(genus, species, simple_status) %>% 
    summarize(agree = sum(as.character(get(prediction)) == as.character(simple_status))/n()
              , frequency_threatened = sum(get(prediction) =="threatened")/n()
              , n_obs= n())
}

simple_pred %>% ggplot(aes(as.factor(simple_status_mu), presum))+
  ggbeeswarm::geom_beeswarm()+
  theme_classic()

simple_pred %>% ggplot(aes(as.factor(simple_status_mu), presum))+
  geom_violin()+
  theme_classic()

picture<-function(df){df %>% ggplot(aes(simple_status, frequency_threatened, color = log(n_obs)))+
  ggbeeswarm::geom_quasirandom( dodge.width = 0.9, alpha = 0.9, size = 1.2)+
  theme_classic() +
  scale_color_viridis_c()}

pdf("figures/threat_prediction_frequency.pdf")
picture(get_a_picture(all_pred, "phigh_stat"))
dev.off()



picture(get_a_picture(predictions_from_summary, "presum"))
# make some maps

resf<-sf::st_as_sf(all_pred
                   , coords = c("lat", "lon")
                   , crs = "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")


resf_sum<-sf::st_as_sf(predictions_from_summary
                   , coords = c("lat", "lon")
                   , crs = "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")


fancy_sf<-resf %>%  group_by(genus, species, simple_status) %>%   
  mutate(agree = sum(as.character(phigh_stat) == as.character(simple_status))/n()
            , frequency_threatened = sum(phigh_stat =="threatened")/n()
            , n_obs= n() ) %>% 
           ungroup() %>% 
         mutate(status_guess = if_else(as.character(simple_status) =="NONE", paste("predicted", as.character(phigh_stat), sep = "_"), as.character(simple_status)))

table(fancy_sf$status_guess)
pdf("figures/prediction_map.pdf")
fancy_sf %>% ggplot(aes(color = status_guess))+
  geom_sf(size = 0.7) +
  theme_classic() +
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "bottom")

pdf("figures/predictions_at_species_level.pdf")
resf_sum %>% filter(!is.na(presum)) %>% 
  mutate(status_guess = if_else(as.character(simple_status) =="NONE", paste("predicted", c("threatened", "secure")[presum], sep = "_"), as.character(simple_status))) %>%  
  ggplot(aes(color =status_guess))+
  geom_sf(size = 0.7) +
  scale_color_brewer(palette = "Dark2") +
  theme_classic() +
  theme(legend.position = "bottom")

dev.off()


resf_sum %>% ggplot(aes(bioclim3_sig, presum, color = simple_status))+geom_point()+theme_classic()
resf_sum %>% ggplot(aes(bioclim4_sig, presum, color = simple_status))+geom_point()+theme_classic()


dev.off()

sum_tab<-get_a_picture(all_pred, "phigh_stat") %>% group_by(genus, species, simple_status, n_obs, frequency_threatened) %>% 
  summarize(n()) %>% arrange(desc(frequency_threatened)) 

sum_tab %>% filter(simple_status =="NONE") %>% group_by(genus) %>% summarize(mft = mean(frequency_threatened), spp = n()) %>% ggplot(aes(spp, mft))+geom_point()+theme_class

all_pred %>% ggplot(aes(slope, phigh, color=simple_status))+
  geom_jitter(height = 0.1, width =10)+
  # geom_smooth(method = "glm", method.args = list(family = "binomial"))+
  theme_classic()+
  scale_color_viridis_d()

pdf("figures/bioclim7_matters.pdf")
all_pred %>% ggplot(aes(bioclim7, phigh, color=simple_status))+
  geom_jitter(alpha = 0.6)+
  # geom_smooth(method = "glm", method.args = list(family = "binomial"))+
  theme_classic()+
  scale_color_viridis_d()+
  labs(x = "annual temperature range", y = "predicted status", color = "input status")
dev.off()

pdf("figures/slope_matters.pdf")
all_pred %>% ggplot(aes(slope, phigh, color=simple_status))+
  geom_jitter(alpha = 0.6)+
  # geom_smooth(method = "glm", method.args = list(family = "binomial"))+
  theme_classic()+
  scale_color_viridis_d()+
  labs(x = "slope", y = "predicted status", color = "input status")
dev.off()

pdf("figures/hottest_month.pdf")
all_pred %>% ggplot(aes(bioclim5, phigh, color=simple_status))+
  geom_jitter(alpha = 0.6)+
  # geom_smooth(method = "glm", method.args = list(family = "binomial"))+
  theme_classic()+
  scale_color_viridis_d()+
  labs(x = "temps of hottest month", y = "predicted status", color = "input status")
dev.off()


head(sum_tab)
View(sum_tab)
