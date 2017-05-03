#==== User section ===================
u_mode   = "R"
u_period = c(1960, 2014)
u_cases  = list(
  "testingMethods - OLS" = list(
    period    = u_period,
    demean    = FALSE,
    firstdiff = FALSE,
    maxsplits = 5, minnbnodes = 3,
    cex       = 0.3, legend.cex = 1.5
  ),
  "testingMethods - Fixed effects" = list(
    period    = u_period,
    demean    = TRUE,
    firstdiff = FALSE,
    maxsplits = 6, minnbnodes = 3,
    cex       = 0.3, legend.cex = 1.5
  ),
  "testingMethods - First-differences" = list(
    period    = u_period,
    demean    = FALSE,
    firstdiff = TRUE,
    maxsplits = 30, minnbnodes = 1,
    cex       = 0.5, legend.cex = 1.0
  ),
  # "testingMethods - OLS (Start 1970)" = list(
  #   period    = c(1970, 2014),
  #   demean    = FALSE,
  #   firstdiff = FALSE,
  #   maxsplits = 5, minnbnodes = 3,
  #   cex       = 0.3, legend.cex = 1.5
  # ),
  "testingMethods - Fixed effects (Start 1970)" = list(
    period    = c(1970, 2014),
    demean    = TRUE,
    firstdiff = FALSE,
    maxsplits = 6, minnbnodes = 3,
    cex       = 0.3, legend.cex = 1.5
  ),
  "testingMethods - Fixed effects (Start 1980)" = list(
    period    = c(1980, 2014),
    demean    = TRUE,
    firstdiff = FALSE,
    maxsplits = 6, minnbnodes = 3,
    cex       = 0.3, legend.cex = 1.5
  ),
  "testingMethods - Fixed effects (Start 1990)" = list(
    period    = c(1990, 2014),
    demean    = TRUE,
    firstdiff = FALSE,
    maxsplits = 6, minnbnodes = 3,
    cex       = 0.3, legend.cex = 1.5
  ),
  # "testingMethods - First-differences (Start 1970)" = list(
  #   period    = c(1970, 2014),
  #   demean    = FALSE,
  #   firstdiff = TRUE,
  #   maxsplits = 30, minnbnodes = 1,
  #   cex       = 0.5, legend.cex = 1.0
  # ),
  # "testingMethods - OLS (End 2010)" = list(
  #   period    = c(1960, 2010),
  #   demean    = FALSE,
  #   firstdiff = FALSE,
  #   maxsplits = 5, minnbnodes = 3,
  #   cex       = 0.3, legend.cex = 1.5
  # ),
  "testingMethods - Fixed effects (End 2010)" = list(
    period    = c(1960, 2010),
    demean    = TRUE,
    firstdiff = FALSE,
    maxsplits = 6, minnbnodes = 3,
    cex       = 0.3, legend.cex = 1.5
  ),
  # "testingMethods - First-differences (End 2010)" = list(
  #   period    = c(1960, 2010),
  #   demean    = FALSE,
  #   firstdiff = TRUE,
  #   maxsplits = 30, minnbnodes = 1,
  #   cex       = 0.5, legend.cex = 1.0
  # )
  "testingMethods - Fixed effects (End 2000)" = list(
    period    = c(1960, 2000),
    demean    = TRUE,
    firstdiff = FALSE,
    maxsplits = 6, minnbnodes = 3,
    cex       = 0.3, legend.cex = 1.5
  )
)


#==== Initialise ========================
source("scripts/user_settings.R")
source("scripts/init.R")
source("scripts/process_data.R")

v_tree  <- list()
v_data  <- list()
v_alloc <- list()


#==== Run experiments ===================
for (k_case in names(u_cases)) {

  print(paste0("Experiment ", k_case))
  
  # Initialise GUIDE file paths and tree options
  outPath  <- file.path("output", k_case)
  treePath <- initpath(outPath)
  #TODO: make this one interactive on demand
  treeOpts <- initTreeOptions(prune      = "1",  # 1=prune by CV, 2=no pruning
                              nbcv       = 10,   # Number of cross-validation steps (i.e. number of sub data spaces)
                              cvtype     = "1",  # 1=mean-based CV tree, 2=median-based CV tree
                              sevalue    = 0.5,  # Standard Error number for pruning [0..1]
                              search     = "2",  # 1=split point from quantiles, 2=use exhaustive search
                              splitfrac  = NA,   # 1=accept default splitting fraction (i.e. frac, where #splits = max(9,fract*n), with n = #cases in node), 2=change it
                              truncmeth  = "3",  # Data truncation method (0=none, 1=node range, 2=+10% node range, 3=global range, 4=2-sided Winsorization)
                              missregval = "2",  # missing regressor values: 1=separate models, 2=impute with means, 3=constant model
                              maxsplits  = u_cases[[k_case]]$maxsplits,     # max. no. split levels
                              minnbnodes = u_cases[[k_case]]$minnbnodes)    # min. node sample size 
  
  # Prepare data
  #---- Prepare data --------
  v_dataShortProc <- data_prepare(v_dataShort %>% spread(variable, value), 
                                  u_variableDefinition, u_cases[[k_case]]$period[1], u_cases[[k_case]]$period[2], 
                                  MISSINGRATIO=0.95, REMOVE_ZEROS=TRUE, FIRSTDIFF=u_cases[[k_case]]$firstdiff, DEMEAN=u_cases[[k_case]]$demean, VERBOSE=FALSE)
  
  v_dataShortProc <- v_dataShortProc %>% 
    gather(variable, value, -iso, -year) %>% 
    mutate(value=ifelse(!is.finite(value), NA, value)) %>% 
    spread(variable, value)
  
  # Redefine variable definition table
  v_variableDefinition <- u_variableDefinition 
  v_variableDefinition$variable <- factor(v_variableDefinition$variable, 
                                          levels=paste(get_renamedVariables(u_variableDefinition, ALL=TRUE)$old), 
                                          labels=paste(get_renamedVariables(u_variableDefinition, ALL=TRUE)$new))
  
  v_variables <- unique(v_dataLong$variable)
  
  # Generate GUIDE input data
  v_dataShortProcGUIDE <- generate_input(v_dataShortProc, v_variableDefinition, 
                                         i_treeType    = "Single tree > LMS - Multilinear", 
                                         i_treeOptions = treeOpts, 
                                         i_fpath       = treePath)
  
  v_data[[k_case]] <- v_dataShortProc
  
  # Run GUIDE
  run(treePath)
  
  # Collect and process results
  v_tree[[k_case]]  <- parse_output(file.path(outPath, "GUIDEfile_out.txt"))
  v_alloc[[k_case]] <- allocateDataToNodes(v_dataShortProc, v_tree[[k_case]])

}

# Save input and results for presentation
save(u_cases, v_data, v_tree, v_alloc, file = file.path("output/dataSlides", paste0("testing_methods_", format(Sys.time(), "%Y-%m-%d"),".RData")))

#==== Plot results ==============
for (k_case in names(u_cases)) {
  tree <- as.fake.rpart.tree(v_tree[[k_case]])
  # plot_tree(tree, 
  #           TITLE=paste0("RT - ", k_case, "\nLMS-Multilinear - Tn:", length(which(v_tree[[k_case]]$nodeType == "Terminal node")), "\n#Countries: ",length(unique(v_data[[k_case]]$iso)), " - period: ",min(v_data[[k_case]]$year),"-",max(v_data[[k_case]]$year)), 
  #           CEX=u_cases[[k_case]]$cex, LEGEND.X = 0, LEGEND.Y = 1.1, LEGEND.CEX=u_cases[[k_case]]$legend.cex, LEGEND.NCOL=2,
  #           FILENAME = file.path("output", paste0(k_case, ".pdf")))
  plot_tree(tree, 
            TITLE=paste0("RT - ", k_case, "\nLMS-Multilinear - Tn:", length(which(v_tree[[k_case]]$nodeType == "Terminal node")), "\n#Countries: ",length(unique(v_data[[k_case]]$iso)), " - period: ",min(v_data[[k_case]]$year),"-",max(v_data[[k_case]]$year)), 
            CEX=NULL, LEGEND.X = 0, LEGEND.Y = 1.1, LEGEND.CEX=0.5, LEGEND.NCOL=2,
            FILENAME = file.path("output", paste0(k_case, ".pdf")))
}

# Plot scatter iso
for (k_case in names(u_cases)) {
  plot_ScatterCountry(v_alloc[[k_case]], v_tree[[k_case]], "DEU" ,"log_P", "log_E_CC", "log_E_CC", PLOT_LABELS = TRUE)
  plot_ScatterCountry_allVars(v_alloc[[k_case]], v_tree[[k_case]], c("DEU", "CHN"), "log_E_CC", PLOT_LABELS = TRUE)
}

# Plot Tnodes
for (kn in as.numeric(v_tree[[k_case]]$nodeID[which(v_tree[[k_case]]$nodeType == "Terminal node")])) {
  tryCatch(
    plot_TNode(v_alloc[[k_case]], kn, v_tree[[k_case]], PLOT_TRANSITION = FALSE),
    error = function(e) {
      cat(paste0("Error with node: ", kn, "\n"))
    }
  )
}

# Plot country pathways
u_countryPathways <- c("CHN", "USA", "DEU", "IND", "JPN", "POL", "GBR", "ZAF", "FRA", "CAN", "AUS", "IDN", "VNM")
for (kc in u_countryPathways) {
  for (k_case in names(u_cases)) {
    plot_ISOPathway(kc, v_alloc[[k_case]], v_tree[[k_case]])
  }
}