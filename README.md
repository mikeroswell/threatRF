# Overview
This repository contains code and data for Roswell and Espíndola "Global 
conservation prioritization approach provides credible results at a regional 
scale" (doi:10.1111/ddi.13969), a manuscript about predicting which unassessed regional taxa are likely to face
conservation threats... using occurrence data, covariates, and random forest 
classifiers.

# General organization

- *code* contains R scripts to download occurrence data and GIS layers, process 
them, and fit the Random Forests. 
- *data* contains all the downloaded and manufactured datasets (these are often 
large) for this project
    - *data/fromR* mainly contains tables generated by the scripts in *code*
    - *data/GIS_downloads* contains raster layers downloaded from various 
    sources
    - the data files for this project are very large. As a result, this 
    repository was created using `git lfs`; see note below
- *figures* contains .pdf files of maps and graphs generated by scripts in 
*code*
- *notes* is a place where I am recording code snippets and notes to myself, 
there is not analysis here

# Workflow within `code/`: 

## Utilities
### data cleanup
1. *tidy_flora.R* regex matching to turn .pdf into a flat file
1. *robust_gbif_namesearch.R* wraps an `rgbif` function to try to get nice 
matches for taxon names without returning synonyms if a valid match exists.
### model fitting etc. 
1. *fix_mod.R* klugey way to handle novel factor levels when using various 
`predict` functions.
1. *RF_tuner.R* specifies how to tune an fit the random forests
1. *RF_setup.R* creates folds for model fitting, cleans up model formulae

## Data download and analysis scripts (may call 1 or more utilities above)
1. *download_gis.R* documents the sources of many of the GIS layers used 
downstream. Created a long time ago and unstable. Do not run
1. *download_occurrences_and_statuses.R* documents the queries in GBIF and 
natureserve. Largely stable but not rerun; the dataset liable to change if 
rerun. 
1. *crunch_GIS.R* Should be rel. stable, all GIS work done in R
1. *fit_RF.R* Fits random forests
1. *graphing_model_outputs.R* generates figures and tabular results


# large files and git lfs
This is the first time I used `git lfs` to handle large files in the repo. 
The files tracked by lfs are most likely handled dysfunctionally if you clone this 
repo and have not installed `git lfs`. For more information on `git lfs`, see
[here](https://git-lfs.github.com/). 

