# code to fit RF models to occurrence data with covariates

library(tidyverse)

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
classed<-tofit_summary_complete %>% filter(simple_status_mu != 1) # 1 corresponds to "NONE"

n_obs<-nrow(classed)
# separate data into train and test
test_rows<-sample(1:n_obs, round(0.2*n_obs))
test<-classed[test_rows, ] # 60 rows
train <- classed[-test_rows, ] # 240 rows

# see if threatened ("2") around twice rel. secure ("3") 
train %>% group_by(simple_status_mu) %>% summarize(n()) # quite close to expected 2:1 in this run
test %>% group_by(simple_status_mu) %>% summarize(n()) # good bit more even

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





my_mod<-as.formula(paste0("as.factor(simple_status_mu) ~ "
                  , paste(names(tofit_summary_complete)[
                    !(grepl("status", names(tofit_summary_complete))
                      | grepl("Rank", names(tofit_summary_complete))
                      | names(tofit_summary_complete) %in% nomatch(get_categorical(train)
                                                                   , get_categorical(test))
                      )
                    # these variables will cause problems
                        ]
                   , collapse= "+")
                  ))


my_mod

source("code/RF_tuner.R")
tictoc::tic()
train_rf<- fit_rf(train, my_mod)
tictoc::toc()

# test that thing!
# first step is fixing factor levels
# test$X2001_2019_change_index_mu<-factor(test$X2001_2019_change_index_mu, levels = levels(classed$X2001_2019_change_index_mu))
test_rf_discrete<-predict(train_rf, test)
sum(test$simple_status_mu == test_rf_discrete)/length(test_rf_discrete) # 73%
# slightly better tha expected from the tuning
train_rf

test_rf_prob<-predict(train_rf, test, type = "prob")

test_rf_prob

# refit with all data

final_rf<-fit_rf(classed, my_mod)

# make predictions on the new data
predict_unclassified<-predict(final_rf, tofit_summary_complete %>% filter(simple_status_mu =="1"))

##################################

summarized_RF_training <- randomForest(
                                       
                                       , data = tofit_summary_complete 
                                       %>% filter(simple_status_mu %in% 2:3) 
                                       %>% droplevels()
                                       # , ytest = c("threat", "secure")
                                       , importance = TRUE
                                       , na.action = na.exclude
                                       , type = "classification"
)

# can classifier see which ones have status?

summarized_RF_training <- randomForest(as.formula(paste0("as.factor(simple_status_mu) ~ "
                                                         , paste(names(tofit_summary_complete)[
                                                           !(grepl("status", names(tofit_summary_complete))
                                                             |grepl("Rank", names(tofit_summary_complete)))][3:74]
                                                           , collapse= "+")))
                                       
                                       , data = tofit_summary_complete 
                                       %>% filter(simple_status_mu %in% 1:3) 
                                       %>% droplevels()
                                       # , ytest = c("threat", "secure")
                                       , importance = TRUE
                                       , na.action = na.exclude
                                       , type = "classification"
)




summarized_RF_training$confusion
varImpPlot(summarized_RF_training)


plot(summarized_RF_training)
data.frame(summarized_RF_training$importance) %>% arrange(desc(MeanDecreaseAccuracy))



summarized_RF_training$importanceSD

presum<-predict(summarized_RF_training, newdata = tofit_summary_complete)
simple_pred<-tofit_summary_complete %>% bind_cols(presum)
predictions_from_summary<-tofit %>% left_join(tofit_summary_complete %>% bind_cols(presum=presum))

get_a_picture <- function(x, prediction){
  print(prediction)
  x %>% 
    group_by(genus, species, simple_status) %>% 
    summarize(agree = sum(as.character(get(prediction)) == as.character(simple_status))/n()
              , frequency_threatened = sum(get(prediction) =="threatened")/n()
              , n_obs= n())
}

simple_pred %>% ggplot(aes(as.factor(simple_status_mu), presum))+
  ggbeeswarm::geom_beeswarm(groupOnX = F)+
  theme_classic()

simple_pred %>% ggplot(aes(as.factor(simple_status_mu), presum))+
  geom_violin()+
  theme_classic()

picture<-function(df){df %>% ggplot(aes(simple_status, frequency_threatened, color = log(n_obs)))+
  ggbeeswarm::geom_quasirandom( dodge.width = 0.9, alpha = 0.9, size = 1.2)+
  theme_classic() +
  scale_color_viridis_c()}

picture(get_a_picture(predictions_from_summary, "presum"))
# make some maps


resf_sum<-sf::st_as_sf(predictions_from_summary
                   , coords = c("lat", "lon")
                   , crs = "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")


resf_sum %>% filter(!is.na(presum)) %>% 
  mutate(status_guess = if_else(as.character(simple_status) =="NONE", paste("predicted", c("threatened", "secure")[presum], sep = "_"), as.character(simple_status))) %>%  
  ggplot(aes(color =status_guess))+
  geom_sf(size = 0.7) +
  scale_color_brewer(palette = "Dark2") +
  theme_classic() +
  theme(legend.position = "bottom")

resf_sum %>% ggplot(aes(maxlat_mu, presum, color = simple_status))+
  geom_jitter(height = 0.2, width =300, alpha = 0.5, size = 0.4)+
  theme_classic()

resf_sum %>% ggplot(aes(bioclim7_sig, presum, color = simple_status))+
  geom_jitter(height = 0.2, width =0.5, alpha = 0.5, size = 0.4)+
  theme_classic()

resf_sum %>% ggplot(aes(bioclim3_mu, presum, color = simple_status))+
  geom_jitter(height = 0.2, width =0.5, alpha = 0.5, size = 0.4)+
  theme_classic()

resf_sum %>% ggplot(aes(bioclim3_sig, presum, color = simple_status))+
  geom_jitter(height = 0.2, width =0.5, alpha = 0.5, size = 0.4)+
  theme_classic()


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
