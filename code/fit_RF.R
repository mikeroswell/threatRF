library(furrr)

# set number of workers for cluster
source("code/RF_setup.R")
cores <- 17


# fit models
future::plan(strategy = "multiprocess", workers = cores)

trees_leps <- map(c( "plant", "lep"), function(tax){
 
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
    
    save(rf, file = paste0("data/fromR/fold_fits_"
                           , tax
                           , "_"
                           , fold, ".RDA"))
    return(rf)
    
  })
 
  print("did the fits")
  # m_assess <- assess_method()
  # print("assess method ran")
  # m_sum <- sum_success(m_assess)
  # print(co)
  # print(exists(fold_fits))
  return(list(tax, fold_fits, outer_folds))
})


save(trees_leps, file="data/fromR/lfs/100_100_fits_20221209.rda")


write.csv(classed, "data/fromR/training_data.csv", row.names = FALSE)
write.csv(no_sing, "data/fromR/all_model_data.csv", row.names = FALSE)
