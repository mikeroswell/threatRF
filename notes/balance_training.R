# unused code to get classifications more balanced

# tofit<-indi %>% dplyr::mutate(lat = sf::st_coordinates(.)[,1],
#                               lon = sf::st_coordinates(.)[,2]) %>% 
#   group_by(genus, species) %>%
#   mutate(maxlat = max(lat, na.rm =T)
#          , minlat = min(lat, na.rm = T)
#          , maxlon = max(lon, na.rm = T)
#          , minlon = min(lon, na.rm = T)
#          , simple_status = factor(if_else(roundedSRank %in% c("S4","S5"), "secure"
#                                           , if_else(roundedSRank =="S3", "intermediate"
#                                                     , if_else(roundedSRank %in% c("S1", "SH"), "critical"
#                                                               , if_else(roundedSRank == "S2", "threatened"
#                                                                         , "NONE")
#                                                               )
#                                                     )
#                                           )
#                                   )
#          ) %>% 
#   filter(!exotic) %>% 
#   sf::st_drop_geometry()





# predictors<-  names(tofit_summary)[names(tofit) %ni% c( "roundedSRank", "roundedNRank", "roundedGRank", "genus", "species", "exotic", "lat", "lon", "simple_status", "geometry", names(tofit)[grepl("OBJECTID*", names(tofit))], names(tofit)[grepl("Descriptio*", names(tofit))] )]
# 
# 
# predictors

# tofit_complete<-tofit %>% drop_na(eval(predictors)) 


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
# summarized_RF_training <- randomForest(as.formula(paste0("as.factor(simple_status_mu) ~ "
#                                                          , paste(names(tofit_summary_complete)[
#                                                            !(grepl("status", names(tofit_summary_complete))
#                                                              |grepl("Rank", names(tofit_summary_complete)))][3:74]
#                                                            , collapse= "+")))
#                                   
#                                   , data = tofit_summary_complete 
#                                   %>% filter(simple_status_mu %in% 2:5) 
#                                   %>% droplevels()
#                                   # , ytest = c("threat", "secure")
#                                   , importance = TRUE
#                                   , na.action = na.exclude
#                                   , type = "classification"
# )
}