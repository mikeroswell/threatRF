##################################
# oldstuff from fitting RF models on plant data

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
