u_mode   = "R"
u_period = c(1970, 2012) # default time frame
# Clark2012 countries (66)
u_iso    = c("AUS", "AUT", "BEL", "CAN", "DNK", "FIN", "FRA", "DEU", "ISL", "IRL", "ISR", "ITA", "JPN", "LUX", "NLD", "NZL", "NOR", "PRT", "ESP", "SWE", "CHE", "GBR", "USA",
             "ALB", "DZA", "ARG", "BGD", "BWA", "BRA", "CHL", "CHN", "COL", "CRI", "CYP", "DOM", "EGY", "GRC", "HND", "HUN", "IND", "IDN", "IRN", "JAM", "KEN", "KOR", "MKD",
             "MYS", "MEX", "MNG", "MAR", "NPL", "PAK", "PER", "PHL", "POL", "ROU", "ZAF", "LKA", "TZA", "THA", "TUR", "URY", "VEN", "VNM", "ZMB", "ZWE")
defineCases  <- function() {list(
  "testingMethods - OLS - AllVars - E_CC" = list(
    period    = u_period,
    dvar      = "E_CC",
    demean    = FALSE,
    firstdiff = FALSE,
    treetype  = "Single tree > LMS - Multilinear", 
    maxsplits = 7, minnbnodes = 3,
    cex       = 0.3, legend.cex = 1.5
  ),
  "testingMethods - Fixed effects - AllVars - E_CC" = list(
    period    = u_period,
    dvar      = "E_CC",
    demean    = TRUE,
    firstdiff = FALSE,
    treetype  = "Single tree > LMS - Multilinear",
    maxsplits = 6, minnbnodes = 3,
    cex       = 0.3, legend.cex = 1.5
  ),
  "testingMethods - First-differences - AllVars - E_CC" = list(
    period    = u_period,
    dvar      = "E_CC",
    demean    = FALSE,
    firstdiff = TRUE,
    treetype  = "Single tree > LMS - Multilinear",
    maxsplits = 8, minnbnodes = 1,
    cex       = 0.5, legend.cex = 1.0
  ),
  "testingMethods - OLS - Clark2012 - E_CC" = list(
    vardef    = generate_variableDefinition(v_dataShort, d="E_CC", n=c("GDPpc", "P","P_Ndep","URB","ManuGDP","Manu_Ex")),
    period    = u_period,
    dvar      = "E_CC",
    demean    = FALSE,
    firstdiff = FALSE,
    treetype  = "Single tree > LMS - Multilinear",
    maxsplits = 5, minnbnodes = 3,
    cex       = 0.3, legend.cex = 1.5
  ),
  "testingMethods - Fixed effects - Clark2012 - E_CC" = list(
    vardef    = generate_variableDefinition(v_dataShort, d="E_CC", n=c("GDPpc", "P","P_Ndep","URB","ManuGDP","Manu_Ex")),
    period    = u_period,
    dvar      = "E_CC",
    demean    = TRUE,
    firstdiff = FALSE,
    treetype  = "Single tree > LMS - Multilinear",
    maxsplits = 5, minnbnodes = 3,
    cex       = 0.3, legend.cex = 1.5
  ),
  "testingMethods - First-differences - Clark2012 - E_CC" = list(
    vardef    = generate_variableDefinition(v_dataShort, d="E_CC", n=c("GDPpc", "P","P_Ndep","URB","ManuGDP","Manu_Ex")),
    period    = u_period,
    dvar      = "E_CC",
    demean    = FALSE,
    firstdiff = TRUE,
    treetype  = "Single tree > LMS - Multilinear",
    maxsplits = 30, minnbnodes = 1,
    cex       = 0.5, legend.cex = 1.0
  )#,
  # "testingMethods - Fixed effects - Simple" = list(
  #   vardef    = generate_variableDefinition(v_dataShort, d="E_CC", n=c("GDPpc", "LEx")),
  #   period    = u_period,
  #   demean    = TRUE,
  #   firstdiff = FALSE,
  #   treetype  = "Single tree > LMS - Multilinear",
  #   maxsplits = 8, minnbnodes = 3,
  #   cex       = 0.3, legend.cex = 1.5
  # ),
  # "testingMethods - OLS-Constant - AllVars" = list(
  #   period    = u_period,
  #   demean    = FALSE,
  #   firstdiff = FALSE,
  #   treetype  = "Single tree > LMS - Constant",
  #   maxsplits = 5, minnbnodes = 3,
  #   cex       = 0.3, legend.cex = 1.5
  # ),
  # "testingMethods - Fixed effects-Constant - AllVars" = list(
  #   period    = u_period,
  #   demean    = TRUE,
  #   firstdiff = FALSE,
  #   treetype  = "Single tree > LMS - Constant",
  #   maxsplits = 6, minnbnodes = 3,
  #   cex       = 0.3, legend.cex = 1.5
  # ),
  # "testingMethods - OLS-Constant - Clark2012 - E_CC" = list(
  #   vardef    = generate_variableDefinition(v_dataShort, d="E_CC", n=c("GDPpc", "P","P_Ndep","URB","ManuGDP","Manu_Ex")),
  #   period    = u_period,
  #   demean    = FALSE,
  #   firstdiff = FALSE,
  #   treetype  = "Single tree > LMS - Constant",
  #   maxsplits = 5, minnbnodes = 3,
  #   cex       = 0.3, legend.cex = 1.5
  # ),
  # "testingMethods - Fixed effects-Constant - Clark2012" = list(
  #   vardef    = generate_variableDefinition(v_dataShort, d="E_CC", n=c("GDPpc", "P","P_Ndep","URB","ManuGDP","Manu_Ex")),
  #   period    = u_period,
  #   demean    = TRUE,
  #   firstdiff = FALSE,
  #   treetype  = "Single tree > LMS - Constant",
  #   maxsplits = 5, minnbnodes = 3,
  #   cex       = 0.3, legend.cex = 1.5
  # ),
  # "testingMethods - Fixed effects - Clark2012Extended" = list(
  #   vardef    = generate_variableDefinition(v_dataShort, d="E_CC", n=c("GDPpc", "P","P_Ndep","URB","ManuGDP","Manu_Ex","LEx","GINI","Debt_Ext","E_CCli","E_CIm", "E_CEx", "EE", "E_RenC", "E_GC")),
  #   period    = u_period,
  #   demean    = TRUE,
  #   firstdiff = FALSE,
  #   treetype  = "Single tree > LMS - Multilinear",
  #   maxsplits = 8, minnbnodes = 3,
  #   cex       = 0.3, legend.cex = 1.5
  # ),
  # "testingMethods - Fixed effects/RandomForest - Clark2012" = list(
  #   vardef    = generate_variableDefinition(v_dataShort, d="E_CC", n=c("GDPpc", "P","P_Ndep","URB","ManuGDP","Manu_Ex")),
  #   period    = u_period,
  #   demean    = TRUE,
  #   firstdiff = FALSE,
  #   treetype  = "Tree ensemble > Random Forest - non-random",
  #   maxsplits = 6, minnbnodes = 3,
  #   cex       = 0.3, legend.cex = 1.5
  # ),
  # "testingMethods - OLS (Start 1970)" = list(
  #   period    = c(1970, 2014),
  #   demean    = FALSE,
  #   firstdiff = FALSE,
  #   maxsplits = 5, minnbnodes = 3,
  #   cex       = 0.3, legend.cex = 1.5
  # ),
  # "testingMethods - Fixed effects - AllVars (Start 1970)" = list(
  #   period    = c(1970, 2014),
  #   demean    = TRUE,
  #   firstdiff = FALSE,
  #   treetype  = "Single tree > LMS - Multilinear",
  #   maxsplits = 6, minnbnodes = 3,
  #   cex       = 0.3, legend.cex = 1.5
  # ),
  # "testingMethods - Fixed effects - AllVars (Start 1980)" = list(
  #   period    = c(1980, 2014),
  #   demean    = TRUE,
  #   firstdiff = FALSE,
  #   treetype  = "Single tree > LMS - Multilinear",
  #   maxsplits = 6, minnbnodes = 3,
  #   cex       = 0.3, legend.cex = 1.5
  # ),
  # "testingMethods - Fixed effects - AllVars (Start 1990)" = list(
  #   period    = c(1990, 2014),
  #   demean    = TRUE,
  #   firstdiff = FALSE,
  #   treetype  = "Single tree > LMS - Multilinear",
  #   maxsplits = 6, minnbnodes = 3,
  #   cex       = 0.3, legend.cex = 1.5
  # ),
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
  # "testingMethods - Fixed effects - AllVars (End 2010)" = list(
  #   period    = c(1960, 2010),
  #   demean    = TRUE,
  #   firstdiff = FALSE,
  #   treetype  = "Single tree > LMS - Multilinear",
  #   maxsplits = 6, minnbnodes = 3,
  #   cex       = 0.3, legend.cex = 1.5
  # ),
  # # "testingMethods - First-differences (End 2010)" = list(
  # #   period    = c(1960, 2010),
  # #   demean    = FALSE,
  # #   firstdiff = TRUE,
  # #   maxsplits = 30, minnbnodes = 1,
  # #   cex       = 0.5, legend.cex = 1.0
  # # )
  # "testingMethods - Fixed effects - AllVars (End 2000)" = list(
  #   period    = c(1960, 2000),
  #   demean    = TRUE,
  #   firstdiff = FALSE,
  #   treetype  = "Single tree > LMS - Multilinear",
  #   maxsplits = 6, minnbnodes = 3,
  #   cex       = 0.3, legend.cex = 1.5
  # )
)
}
u_cases <- defineCases()

#==== Initialise ========================
source("scripts/user_settings.R")
source("scripts/init.R")
source("scripts/process_data.R")

v_tree  <- list()
v_data  <- list()
v_alloc <- list()
v_allocOriginal <- list()


#==== Check data ========================



#==== Run experiments ===================
for (k_case in names(u_cases)) {

  print(paste0("Experiment ", k_case))
  
  # Initialise GUIDE file paths and tree options
  outPath  <- file.path("output/testingMethods/RT models", k_case)
  treePath <- initpath(outPath)
  dir.create(treePath$outPath, recursive = TRUE)
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
  if (!is.null(u_cases[[k_case]]$vardef)) {
    tmp_vardef <- u_cases[[k_case]]$vardef
    tmp_vardef$type[which(tmp_vardef$variable %in% c("E_CC", "E_CCGDP", "E_CCP"))] <- "x"
    tmp_vardef$type[which(tmp_vardef$variable == u_cases[[k_case]]$dvar)] <- "d"
  } else {
    tmp_vardef <- u_variableDefinition
    tmp_vardef$type[which(tmp_vardef$variable %in% c("E_CC", "E_CCGDP", "E_CCP"))] <- "x"
    tmp_vardef$type[which(tmp_vardef$variable == u_cases[[k_case]]$dvar)] <- "d"
  }
  
  v_dataShortProc <- data_prepare(v_dataShort %>% spread(variable, value), 
                                  tmp_vardef, 
                                  u_cases[[k_case]]$period[1], u_cases[[k_case]]$period[2], 
                                  MISSINGRATIO = 0.95, REMOVE_ZEROS = TRUE, 
                                  FIRSTDIFF = u_cases[[k_case]]$firstdiff, 
                                  DEMEAN    = u_cases[[k_case]]$demean, 
                                  VERBOSE   = FALSE)
  
  v_dataShortProc$data <- v_dataShortProc$data %>% 
    gather(variable, value, -iso, -year) %>% 
    mutate(value=ifelse(!is.finite(value), NA, value)) %>% 
    spread(variable, value)
  
  # Redefine variable definition table
  v_variableDefinition <- tmp_vardef 
  v_variableDefinition$variable <- factor(v_variableDefinition$variable, 
                                          levels=paste(get_renamedVariables(tmp_vardef, ALL=TRUE)$old), 
                                          labels=paste(get_renamedVariables(tmp_vardef, ALL=TRUE)$new))
  
  v_variables <- unique(v_dataLong$variable)
  
  # Generate GUIDE input data
  v_dataShortProcGUIDE <- generate_input(v_dataShortProc$data, v_variableDefinition, 
                                         i_treeType    = u_cases[[k_case]]$treetype,  # "Single tree > LMS - Multilinear"
                                         i_treeOptions = treeOpts, 
                                         i_fpath       = treePath)
  
  v_data[[k_case]] <- v_dataShortProc
  
  # Run GUIDE
  run(treePath)
  
  # Collect and process results
  v_tree[[k_case]]  <- parse_output(file.path(outPath, "GUIDEfile_out.txt"))
  v_alloc[[k_case]] <- allocateDataToNodes(v_dataShortProc$data, v_tree[[k_case]]) 
  if (!is.null(v_dataShortProc$datamean)) {
    v_allocOriginal[[k_case]] <- v_alloc[[k_case]] %>% 
      gather(variable, value, -iso,-year,-tnode) %>% 
      mutate(original_variable=gsub("log_","",variable)) %>% 
      left_join( v_dataShortProc$datamean, by=c("iso","original_variable"="variable")) %>% 
      mutate(original_value=exp(value+mean))
  } else {
    v_allocOriginal[[k_case]] <- v_alloc[[k_case]] %>% 
      gather(variable, value, -iso,-year,-tnode) %>% 
      mutate(original_variable=gsub("log_","",variable)) %>% 
      mutate(original_value=exp(value))
  }

}

# Save input and results for presentation
#save(u_cases, v_data, v_tree, v_alloc, file = file.path("output/testingMethods/data", paste0("testing_methods_", format(Sys.time(), "%Y-%m-%d"),".RData")))
load(file.path("output/testingMethods/data", paste0("testing_methods_", format(Sys.time(), "%Y-%m-%d"),".RData")))

#==== Plot results ==============
for (k_case in names(u_cases)) {
  print(k_case)
  tree <- as.fake.rpart.tree(v_tree[[k_case]])
  # plot_tree(tree, 
  #           TITLE=paste0("RT - ", k_case, "\nLMS-Multilinear - Tn:", length(which(v_tree[[k_case]]$nodeType == "Terminal node")), "\n#Countries: ",length(unique(v_data[[k_case]]$iso)), " - period: ",min(v_data[[k_case]]$year),"-",max(v_data[[k_case]]$year)), 
  #           CEX=u_cases[[k_case]]$cex, LEGEND.X = 0, LEGEND.Y = 1.1, LEGEND.CEX=u_cases[[k_case]]$legend.cex, LEGEND.NCOL=2,
  #           FILENAME = file.path("output", paste0(k_case, ".pdf")))
  if (dim(tree$frame)[1] != 0) {
    plot_tree(tree, 
              TITLE=paste0("RT - ", k_case, "\nLMS-Multilinear - Tn:", length(which(v_tree[[k_case]]$nodeType == "Terminal node")), "\n#Countries: ",length(unique(v_data[[k_case]]$data$iso)), " - period: ",min(v_data[[k_case]]$data$year),"-",max(v_data[[k_case]]$data$year)), 
              CEX=NULL, LEGEND.X = 0, LEGEND.Y = 1.1, LEGEND.CEX=0.5, LEGEND.NCOL=2,
              FILENAME = file.path("output/testingMethods/Plots Trees/", paste0(k_case, ".pdf")))
    plot_tree(tree, 
              TITLE=paste0("RT - ", k_case, "\nLMS-Multilinear - Tn:", length(which(v_tree[[k_case]]$nodeType == "Terminal node")), "\n#Countries: ",length(unique(v_data[[k_case]]$data$iso)), " - period: ",min(v_data[[k_case]]$data$year),"-",max(v_data[[k_case]]$data$year)), 
              CEX=NULL, LEGEND.X = 0, LEGEND.Y = 1.1, LEGEND.CEX=0.5, LEGEND.NCOL=2,
              FILENAME = file.path("output/testingMethods/Plots Trees/", paste0(k_case, ".png")))
  }
}
# Plot country pathways
for (k_case in names(u_cases)) {
  png(filename=file.path("output/testingMethods/Plots country pathways/", paste0(k_case, ".png")), width=1500, height=1500)
  plot_ISOPathways(c("CHN", "USA"),v_alloc[[k_case]], v_tree[[k_case]], export="test.gexf")
  dev.off()
}

# Plot scatter iso
for (k_case in names(u_cases)) {
  print(k_case)
  if (dim(v_tree[[k_case]])[1] != 0) {
    yvar=ifelse(k_case != "testingMethods - Fixed effects - Simple", "log_P", "log_LEx")
    png(filename = file.path("output/testingMethods/Plots ScatterIso/", paste0("scatterIso_GDPpcVSPop_", k_case, ".png")), width=1000, height=1000, res=100)
    plot_ScatterCountry(v_alloc[[k_case]], v_tree[[k_case]], "DEU", "log_GDPpc",yvar, "log_E_CC", PLOT_LABELS = TRUE)
    dev.off()
    png(filename = file.path("output/testingMethods/Plots ScatterIso/", paste0("scatterIso_AllVars_", k_case, ".png")), width=1000, height=1000, res=100)
    plot_ScatterCountry_allVars(v_alloc[[k_case]], v_tree[[k_case]], c("DEU", "CHN"), "log_E_CC", PLOT_LABELS = TRUE)
    dev.off()
  }
}

# Plot Tnodes
for (k_case in names(u_cases)[15]){#[c(3,5,6,8,10,11,12)]) {
  print(k_case)
  dir.create(file.path("output/testingMethods/Plots Tnode/", k_case))
  if (dim(tree$frame)[1] != 0) {
    for (kn in as.numeric(v_tree[[k_case]]$nodeID[which(v_tree[[k_case]]$nodeType == "Terminal node")])) {
      png(filename = file.path("output/testingMethods/Plots Tnode/", k_case, paste0("tnode_", k_case, "_", kn, ".png")), width=1000, height=1000, res=100)
      tryCatch(
        plot_TNode(v_alloc[[k_case]], kn, v_tree[[k_case]], PLOT_TRANSITION = FALSE),
        error = function(e) {
          cat(paste0("Error with node: ", kn, "\n"))
        }
      )
      dev.off()
    }
  }
}

# Plot country pathways
u_countryPathways <- c("CHN", "USA", "DEU", "IND", "JPN", "POL", "GBR", "ZAF", "FRA", "CAN", "AUS", "IDN", "VNM")
for (k_case in names(u_cases)[c(3,5,6,8,10,11,12)]) {
  print(k_case)
  dir.create(file.path("output/testingMethods/Plots country pathways/", k_case))
  for (kc in u_countryPathways) {
    png(filename = file.path("output/testingMethods/Plots country pathways/", k_case, paste0("tnode_", k_case, "_", kn, ".png")), width=1000, height=1000, res=100)
    plot_ISOPathway(kc, v_alloc[[k_case]], v_tree[[k_case]])
    dev.off()
  }
}

plot_TNode(v_alloc[[k_case]], 31, v_tree[[k_case]], PLOT_TRANSITION = FALSE)
