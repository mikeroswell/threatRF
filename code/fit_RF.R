library(furrr)

# set number of workers for cluster
source("code/RF_setup.R")
cores<-16


# fit models
future::plan(strategy = "multiprocess", workers = cores)

trees_leps<-map(c("plant", "lep"), function(tax){
 
  main <- get(paste0("classed.", tax ))
  classy <- dropper(main)
  outer_folds <- folder(classy, "simple_status_mu")
  save(outer_folds, file = "data/fromR/outerFolds.RDA")
  # fit models
  fold_fits <- furrr::future_map(1:length(outer_folds), function(fold){ # 
    tic()

    rf <- fit_rf(formu = my_mod
                , data = classy[outer_folds[[fold]], ]
                , sampling = NULL
                , tuneMethod = "repeatedcv"
                , repeats = 10
    )


    print(toc())
    
   
    return(rf)
    
  })
  save(rf, file = past0("data/fromR/fold_fits_", tax, ".RDA"))
  print("did the fits")
  # m_assess <- assess_method()
  # print("assess method ran")
  # m_sum <- sum_success(m_assess)
  # print(co)
  # print(exists(fold_fits))
  return(list(tax, fold_fits, outer_folds))
})


save(trees_leps, file="data/fromR/lfs/100_100_fits_20220917_espindolab.rda")


write.csv(classed, "data/fromR/training_data.csv", row.names = FALSE)
