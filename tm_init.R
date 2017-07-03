#==== Initialise ========================
source("scripts/init.R")
library(foreach)
if (u_doParallel) {
  library(doParallel)
  library(doSNOW)
  
  # Define umber of CPU cores
  if (as.logical(Sys.info()["sysname"] == "Linux"))   registerDoMC(4)
  if (as.logical(Sys.info()["sysname"] == "Windows")) registerDoSNOW(makeCluster(4))
}

source("tm_prepare_data.R")
source("tm_parse_results.R")
source("tm_plot_experiment.R")
source("tm_plot_modelFittingPerformance.R")

# Process data
if (u_processData | length(which(ls() == "v_dataShort")) == 0) source("scripts/process_data.R")

defineCases  <- function(projName, selectedDepVars, selectedIndepVars, period, iso, treemod, missingratio, SENSITIVITY_ANALYSIS=FALSE) {
  
  #===== Rank coal consuming countries by decreasing order ===================
  tmp <- v_dataShort %>% 
    dplyr::filter(variable == "E_CC") %>% 
    dplyr::select(-variable) %>% 
    dplyr::group_by(iso) %>% 
    dplyr::arrange(year) %>% 
    dplyr::summarize(value=sum(value, na.rm=TRUE)) %>% 
    dplyr::ungroup() %>% 
    dplyr::filter(value != 0) %>% 
    dplyr::arrange(desc(value))
  
  iso_lvls = tmp$iso
  
  getCountries <- function(startYear, selectedVars, yearcumsum=2010){
    tmp <- v_dataShort %>% 
      dplyr::filter(iso %in% iso_lvls, variable %in% selectedVars) %>% 
      dplyr::group_by(variable, iso) %>% 
      dplyr::filter(!is.na(value) & value != 0) %>% 
      dplyr::summarise(min_year=min(year), max_year=max(year)) %>% 
      dplyr::ungroup() %>% 
      dplyr::left_join(
        v_dataShort %>% 
          dplyr::filter(variable == "E_CC", year == yearcumsum) %>% 
          dplyr::select(iso, value)) %>% 
      dplyr::mutate(iso = factor(iso, levels=iso_lvls, ordered=TRUE)) %>% 
      dplyr::filter(min_year <= startYear) %>% 
      dplyr::select(-min_year, -max_year) %>% 
      dplyr::spread(variable, value) %>% 
      dplyr::gather(variable, value, -iso) %>%     
      dplyr::group_by(iso) %>% 
      dplyr::summarise(keep=ifelse(!is.na(sum(value)), 1, 0)) %>% 
      dplyr::ungroup() %>% 
      dplyr::filter(keep == 1) %>% 
      dplyr::select(iso) %>% 
      dplyr::left_join(
        v_dataShort %>% 
          dplyr::filter(variable == "E_CC", year == yearcumsum) %>% 
          dplyr::select(iso, value))
    
    return(tmp$iso)
    
  } 
  
  
  out <- list()
  
  for (kp in names(period)) {
    for (kiso in names(iso)) {
      for (kdv in selectedDepVars) {
        for (kiv in names(selectedIndepVars)) {
          for (ktm in treemod) {
            for (kmr in missingratio) {
              if (ktm == "Std-MLinROLS") {
                tmp_demean    <- FALSE
                tmp_firstdiff <- FALSE
                treetype      <- "Single tree > LMS - Multilinear"
              }
              if (ktm == "FE-MLinROLS") {
                tmp_demean    <- TRUE
                tmp_firstdiff <- FALSE
                treetype      <- "Single tree > LMS - Multilinear"
              }
              if (ktm == "FD-MLinROLS") {
                tmp_demean    <- FALSE
                tmp_firstdiff <- TRUE
                treetype      <- "Single tree > LMS - Multilinear"
              }
              if (ktm == "Std-LinROLS") {
                tmp_demean    <- FALSE
                tmp_firstdiff <- FALSE
                treetype      <- "Single tree > LMS - Best simple linear"
              }
              if (ktm == "FE-LinROLS") {
                tmp_demean    <- TRUE
                tmp_firstdiff <- FALSE
                treetype      <- "Single tree > LMS - Best simple linear"
              }
              if (ktm == "FD-LinROLS") {
                tmp_demean    <- FALSE
                tmp_firstdiff <- TRUE
                treetype      <- "Single tree > LMS - Best simple linear"
              }
              if (ktm == "Std-ConROLS") {
                tmp_demean    <- FALSE
                tmp_firstdiff <- FALSE
                treetype      <- "Single tree > LMS - Constant"
              }
              if (ktm == "FE-ConROLS") {
                tmp_demean    <- TRUE
                tmp_firstdiff <- FALSE
                treetype      <- "Single tree > LMS - Constant"
              }
              if (ktm == "FD-ConROLS") {
                tmp_demean    <- FALSE
                tmp_firstdiff <- TRUE
                treetype      <- "Single tree > LMS - Constant"
              }
              
              if (kiv != "Clark2012") {
                tmp_iso <- iso[[kiso]]
              } else {
                tmp_iso <- u_iso_clark2012
              }
              
              casename <- paste0(projName, " - ", kdv, "-", kiv, "-", ktm, "-", period[[kp]][1], "-", period[[kp]][2], "-", kiso, "-MR", kmr)
              
              print(casename)
              
              
              
              out[[casename]] <- 
                list(
                  vardef    = generate_variableDefinition(v_dataShort, d=kdv, n=selectedIndepVars[[kiv]][which(selectedIndepVars[[kiv]] != kdv)]),
                  period    = period[[kp]],
                  iso       = tmp_iso,
                  dvar      = kdv,
                  log       = TRUE,
                  demean    = tmp_demean,
                  firstdiff = tmp_firstdiff,
                  treetype  = treetype,
                  missratio = kmr,
                  maxsplits = 6, minnbnodes = 3,
                  cex       = 0.3, legend.cex = 1.5
                )
              
              
              if (SENSITIVITY_ANALYSIS) {
                for (ky in 0:2) { # Start year
                  for (kd in c(5,6,7)) {
                    for (kn in c(3,10)) {
                      if (ky != 0 && kd != 6 && ky != 3){
                        print(paste0(casename, "-md", kd, "-mn", kn))
                        out[[paste0(casename, "-md", kd, "-mn", kn)]] <- 
                          list(
                            vardef    = generate_variableDefinition(v_dataShort, d=kdv, n=selectedIndepVars[[kiv]][which(selectedIndepVars[[kiv]] != kdv)]),
                            period    = c(period[[kp]][1]+ky, period[[kp]][2]),
                            iso       = tmp_iso,
                            dvar      = kdv,
                            log       = TRUE,
                            demean    = tmp_demean,
                            firstdiff = tmp_firstdiff,
                            treetype  = treetype,
                            missratio = kmr,
                            maxsplits = kd, minnbnodes = kn,
                            cex       = 0.3, legend.cex = 1.5
                          )
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
    
  }
  return(out)
  
}
u_cases <- defineCases(testingMethodName, u_selectedDepVars,u_selectedIndepVars,u_period,u_isos,u_treeModels, u_missingRatio, SENSITIVITY_ANALYSIS=FALSE)

# Initialise variables
v_tree  <- list()
v_data  <- list()
v_alloc <- list()
v_allocOriginal <- list()
v_regdata <- list()

v_runtime <- c()

# Create directory structure
dir.create(file.path("output", testingMethodName, "Plots data"))
dir.create(file.path("output", testingMethodName, "Plots Trees"))
dir.create(file.path("output", testingMethodName, "Plots country pathways"))
dir.create(file.path("output", testingMethodName, "Plots fitting performance"))
dir.create(file.path("output", testingMethodName, "check_data"), recursive = TRUE)