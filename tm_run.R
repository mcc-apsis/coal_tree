#==== Run experiments ===================
if (u_rerun || !file.exists(file.path("output", testingMethodName, "data", paste0(testingMethodName, "_GUIDEresults_", format(Sys.time(), "%Y-%m-%d"),".RData")))) {
  t1 <- Sys.time()
  
  allcases <- lapply(names(u_cases), function(x) paste0(x,"~",paste(0:Ncases)))
  allcases <- allcases[[1]]
  #--- PARALLELISED LOOP -------------------
  results <- foreach(k=c(1:length(unlist(allcases))), 
                     .packages = c("guidr", "dplyr", "tidyr", "igraph", "ggplot2", "magrittr", "rpart", "rpart.plot"), 
                     .export   = c("testingMethodName", "Ncases", "allcases",
                                   "u_cases", "u_countryPathwaysPlots", "u_modelFittingPerformancePlots", "u_remove1obs", "u_timeseriesTreeInfoPlots", 
                                   "u_treePlots",
                                   "tm_prepare_data",  "tm_parse_results", "tm_plot_experiment", "tm_plot_modelFittingPerformance",
                                   "plot_timeseries_wTreeinfo", 
                                   "v_dataShort", "u_variableDefinition"),
                     .inorder = FALSE,
                     .verbose = TRUE) %dopar% {

    t11 <- Sys.time()                       
                                              
    k_tm   <- strsplit(unlist(allcases)[k], "~", fixed=TRUE)[[1]][1]
    k_case <- strsplit(unlist(allcases)[k], "~", fixed=TRUE)[[1]][2]

    k_caseName <- ifelse(nchar(k_case) < nchar(Ncases), paste0(paste(rep("0", nchar(Ncases)-nchar(k_case)), collapse=""), k_case), paste0(k_case))
    
    cat(paste0(k_tm, " - Experiment ", k_caseName , ": \n"))
    
    
    # Set tree options
    treeOpts <- initTreeOptions(prune      = "1",  # 1=prune by CV, 2=no pruning
                                nbcv       = 10,   # Number of cross-validation steps (i.e. number of sub data spaces)
                                cvtype     = "1",  # 1=mean-based CV tree, 2=median-based CV tree
                                sevalue    = 0.5,  # Standard Error number for pruning [0..1]
                                search     = "2",  # 1=split point from quantiles, 2=use exhaustive search
                                splitfrac  = NA,   # 1=accept default splitting fraction (i.e. frac, where #splits = max(9,fract*n), with n = #cases in node), 2=change it
                                truncmeth  = "3",  # Data truncation method (0=none, 1=node range, 2=+10% node range, 3=global range (default), 4=2-sided Winsorization)
                                missregval = "2",  # Missing regressor values: 1=separate models, 2=impute with means (default), 3=constant model
                                maxsplits  = u_cases[[k_tm]]$maxsplits,     # max. no. split levels
                                minnbnodes = u_cases[[k_tm]]$minnbnodes)    # min. node sample size 
    
    # Prepare data
    prep_dat             <- tm_prepare_data(u_cases[[k_tm]], v_dataShort)
    v_dataShortProc      <- prep_dat$data
    v_variableDefinition <- prep_dat$vardef
    rm("prep_dat")

    #---- Ramdom sample case -------
    # Initialise GUIDE file paths and tree options
    outPath  <- file.path("output", testingMethodName, "RT models", paste0(k_tm, " - T", k_caseName))
    treePath <- initpath(outPath)
    dir.create(treePath$outPath, recursive = TRUE)
    
    # Sample data (remove observations)
    miss_obs <- ""
    if (k_case != 0) {
      # Get number of observations in default tree
      Nobs <- dim(v_dataShortProc$data)[1]
      
      if (u_remove1obs) {
        # Remove 1 observation
        tmp_data_sample      <- v_dataShortProc
        tmp_data_sample$data <- tmp_data_sample$data %>% sample_n(Nobs-1)
        v_data               <- tmp_data_sample
        
        miss_obs <- unique((v_dataShortProc$data %>% mutate(iy=paste0(iso,"-",year)))$iy)[
          which(!unique((v_dataShortProc$data %>% mutate(iy=paste0(iso,"-",year)))$iy) %in% 
                 unique((tmp_data_sample$data %>% mutate(iy=paste0(iso,"-",year)))$iy))]
        
      } else {
        # Remove 5% of observations
        tmp_data_sample      <- v_dataShortProc
        tmp_data_sample$data <- tmp_data_sample$data %>% sample_frac(0.95)
        v_data               <- tmp_data_sample
      }
      
    } else {
      v_data <- v_dataShortProc
    }
    
    # Generate GUIDE input data
    v_dataShortProcGUIDE <- generate_input(v_data$data, v_variableDefinition, 
                                           i_treeType    = u_cases[[k_tm]]$treetype, 
                                           i_treeOptions = treeOpts, 
                                           i_fpath       = treePath)
    
    # Run GUIDE
    run_guide(treePath)
    
    # Collect and process results
    res <- tm_parse_results(file.path(outPath, "GUIDEfile_out.txt"), v_variableDefinition, v_data)
    
    # Save data in folder
    guide_data <- list(
      case    = u_cases[[k_tm]],
      data    = v_data, 
      tree    = res$tree,
      pode    = res$pode,
      secbest = res$secbest,
      alloc   = res$alloc, 
      regdata = res$regdata,
      runtime = Sys.time() - t11
    )
    save(guide_data, file = file.path(outPath, paste0(testingMethodName, "_GUIDEdata_", format(Sys.time(), "%Y-%m-%d"),".RData")))
    
    cat(paste0("  It took ", difftime(Sys.time(), t11, units = "mins"), " (min) to prepare data, run GUIDE and parse results.\n"))
    
    # Plot results
    t2 <- Sys.time()
    
    cat(paste0("  Plotting Experiment ", k_caseName , "\n"))
    
    tm_plot_experiment(k_tm, k_caseName, res$tree, v_data, res$alloc, res$regdata, res$pode, res$secbest, u_cases[[k_tm]])
    
    cat(paste0("    It took ",  difftime(Sys.time(), t2, units = "mins"), " (min) to plot this experiment.\n"))
    
    guide_data
      
  } # end cases loop
  
  # Save input and results for presentation
  dir.create(file.path("output", testingMethodName, "data"))
  save(results, file = file.path("output", testingMethodName, "data", paste0(testingMethodName, "_GUIDEresults_", format(Sys.time(), "%Y-%m-%d"),".RData")))
  
  cat(paste0("It took ",  difftime(Sys.time(), t1, units = "mins"), " (min) to perform all experiments.\n"))
} else {
  load(file.path("output", testingMethodName, "data", paste0(testingMethodName, "_GUIDEresults_", format(Sys.time(), "%Y-%m-%d"),".RData")))
  cat(paste0("It took ",  difftime(Sys.time(), t1, units = "mins"), " (min) to load all experiments.\n"))
}