### climatedata::get_chelsa with updated URL (and no ftp)

get_chelsa <- function (type = "bioclim", layer = 1:19, period, model_string,
          scenario_string, future_years, output_dir)
{
  stopifnot(layer %in% 1:19, type == "bioclim", period %in%
              c("past", "current", "future"))
  if (missing(output_dir)) {
    output_dir <- getwd()
  }
  else {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }
  if (period == "future") {
    stopifnot(future_years %in% c("2041-2060", "2061-2080"),
              scenario_string %in% c("rcp26", "rcp45", "rcp60",
                                     "rcp85"), model_string %in% c("ACCESS1-0", "BNU-ESM",
                                                                   "CCSM4", "CESM1-BGC", "CESM1-CAM5", "CMCC-CMS",
                                                                   "CMCC-CM", "CNRM-CM5", "CSIRO-Mk3-6-0", "CanESM2",
                                                                   "FGOALS-g2", "FIO-ESM", "GFDL-CM3", "GFDL-ESM2G",
                                                                   "GFDL-ESM2M", "GISS-E2-H-CC", "GISS-E2-H", "GISS-E2-R-CC",
                                                                   "GISS-E2-R", "HadGEM2-AO", "HadGEM2-CC", "IPSL-CM5A-LR",
                                                                   "IPSL-CM5A-MR", "MIROC-ESM-CHEM", "MIROC-ESM",
                                                                   "MIROC5", "MPI-ESM-LR", "MPI-ESM-MR", "MRI-CGCM3",
                                                                   "MRI-ESM1", "NorESM1-M", "bcc-csm1-1", "inmcm4"))
  }
  layerf <- sprintf("%02d", layer)
  stopifnot(layerf %in% sprintf("%02d", 1:19))
  if (period == "past") {
    stopifnot(model_string %in% c("CCSM4", "CNRM-CM5", "FGOALS-g2",
                                  "IPSL-CM5A-LR", "MIROC-ESM", "MPI-ESM-P", "MRI-CGCM3"))
    if (missing(scenario_string)) {
      cat("Argument scenario_string missing. Assuming pmip3 scenario",
          "\n")
      scenario_string <- "pmip3"
    }
    path <- paste0(normalizePath(output_dir), "/past/")
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
    for (i in layerf) {
      for (model_s in model_string) {
        out_layer <- glue::glue("CHELSA_PMIP_{model_s}_BIO_{i}.tif")
        layer_url <- glue::glue("https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V1/pmip3/bioclim/{out_layer}")
        file_path <- paste0(path, out_layer)
        if (!file.exists(file_path)) {
          download.file(layer_url, file_path)
        }
      }
    }
    return(stack(list.files(path, full.names = TRUE)))
  }
  if (period == "current") {
    path <- paste0(normalizePath(output_dir), "/current/")
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
    for (i in layerf) {
      out_layer <- glue::glue("CHELSA_bio10_{i}.tif")
      layer_url <- glue::glue("https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V1/climatologies/bio/{out_layer}")
      file_path <- paste0(path, out_layer)
      if (!file.exists(file_path)) {
        download.file(layer_url, file_path)
      }
    }
    return(stack(list.files(path, full.names = TRUE)))
  }
  if (period == "future") {
    path <- paste0(normalizePath(output_dir), "/future/")
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
    for (future_y in future_years) {
      for (scenario_s in scenario_string) {
        for (model_s in model_string) {
          for (i in layer) {
            layer_name <- glue::glue("CHELSA_bio_mon_{model_s}_{scenario_s}_r1i1p1_g025.nc_{i}_{future_y}_V1.2.tif")
            layer_url <- glue::glue("https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V1/cmip5/{future_y}/bio/{layer_name}")
            file_path <- paste0(path, layer_name)
            if (!file.exists(file_path)) {
              download.file(layer_url, file_path)
            }
          }
        }
      }
    }
    print("Nothing returned")
    return(NULL)
  }
}

#e.g. path to one file
#https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/1981-2010/bio/CHELSA_ai_1981-2010_V.2.1.tif
