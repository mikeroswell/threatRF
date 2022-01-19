# code to fit RF models to occurrence data with covariates

library(tidyverse)
library(randomForest)


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

# set random seed so results are same each time
set.seed(888)

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
train_rf_up<- fit_rf(train, my_mod, sampling="up")
train_rf_down<- fit_rf(train, my_mod, sampling = "down")

train_rf_orig<-fit_rf(train, my_mod)
tictoc::toc()

# test that thing!
train_rf_up$finalModel
test_rf_discrete<-predict(train_rf_up, test)
sum(test$simple_status_mu == test_rf_discrete)/length(test_rf_discrete) # 73%

train_rf_down$finalModel
test_rf_discrete_d<-predict(train_rf_down, test)
sum(test$simple_status_mu == test_rf_discrete_d)/length(test_rf_discrete_d) # 75

train_rf_orig$finalModel
test_rf_discrete_o<-predict(train_rf_orig, test)
sum(test$simple_status_mu == test_rf_discrete_o)/length(test_rf_discrete_o) # 7%





# slightly better than expected from the tuning
confusion<-test %>% ungroup() %>% mutate(prediction=test_rf_discrete, orig=test_rf_discrete_o, down = test_rf_discrete_d)
confusion %>%
  mutate(gotit_up = case_when(simple_status_mu == prediction~1, TRUE ~ 0 )
         , gotit_orig = case_when(simple_status_mu == orig~1, TRUE  ~ 0 )
         , gotit_down = case_when(simple_status_mu == down~1, TRUE  ~ 0 )
         ) %>%  
  group_by(simple_status_mu) %>% 
  summarize( e_rate_up = sum(gotit_up)/n()
            , e_rate_orig = sum(gotit_orig)/n()
            ,  e_rate_down = sum(gotit_orig)/n()
            , tot = n())

# but isn't much better in test. Good to go with it for now? Maybe. 


# refit with all data? If so, need to make sure hyperparameters are saved etc.,
# otherwise this can be a very different model
# final_rf<-fit_rf(classed, my_mod)

# plot(varImp(final_rf))
# variable importance plots!
pdf("figures/plant_importance.pdf")
varImpPlot(final_rf$finalModel)
dev.off()

# make predictions on the new data
predict_unclassified<-predict(final_rf, tofit_summary_complete %>% 
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

rast_pred <- w_preds %>%
  filter(had_status =="predicted") %>% 
  raster::rasterize(
    y = base_rast
    , fun = function(x, ...){
          if_else(sum(na.omit(x))>0
                  , sum(na.omit(x) > threat_thres)/sum(na.omit(x) > 0)
                  , 0)
      }
    , field = "p.threatened"
    , na.rm = FALSE)

  

pdf("figures/example_raster_with_proportion_probably_threatened.pdf")

ggplot()+
  geom_tile(data = as.data.frame(as(rast_pred, "SpatialPixelsDataFrame"))
            , aes(x = x, y = y, fill = layer))+
  scale_fill_viridis_c() +
  coord_equal() +
  theme_void() +
  labs(fill = "proportion observations \nwith predicted probability \nof being threatened \n>90% ")

dev.off()

w_preds %>%
  sf::st_drop_geometry() %>% 
  filter(had_status == "predicted") %>% 
  group_by(genus, species) %>% 
  summarize(threat_pred = mean(p.threatened)) %>% 
  arrange(desc(threat_pred))

