u_mode   = "R"

source("scripts/user_settings.R")
source("scripts/init.R")
source("scripts/process_data.R")

testingMethodName = "testingMethods5"
Ncases   = 500
u_period = list(
  "LongTerm40y" = c(1971, 2012),
  "LongTerm20y" = c(1991, 2012))
u_selectedDepVars = c("E_CC")
u_selectedIndepVars = list(
  "Clark2012"   = c("GDPpc", "P", "P_Ndep", "URB", "Manu", "Manu_Ex"),
  "EnhancedSet" = c("E_CIm", "E_CEx", "E_CP", 
                    "GDPpc", "P", "P_Ndep", 
                    "URB", "LEx", "EE", 
                    "GDP_Ag", "GDP_Ind", "GDP_Ser", "GDP_Tra"),
  "EnhancedSetNoSer" = c("E_CIm", "E_CEx", "E_CP", 
                    "GDPpc", "P", "P_Ndep", 
                    "URB", "LEx", "EE", 
                    "GDP_Ag", "GDP_Ind", "GDP_Tra"),
  "EnhancedSetNoAgr" = c("E_CIm", "E_CEx", "E_CP", 
                         "GDPpc", "P", "P_Ndep", 
                         "URB", "LEx", "EE", 
                         "GDP_Ser", "GDP_Ind", "GDP_Tra"))
u_treeModels = c("FE-MLinROLS")
# 80 countries
u_iso = c("CHN","USA", "DEU", "IND", "JPN", "POL", "GBR", "ZAF", "FRA", "KOR", "TUR", "CAN", "AUS", "BEL", "ITA", "BRA", "ESP", "IDN",
                     "VNM", "THA", "HUN", "ROU", "AUT", "BGR", "NLD", "PAK", "MEX", "COL", "SWE", "FIN", "ZWE", "NOR", "NZL", "DNK", "GRC", "PHL", 
                     "MYS", "CHL", "CHE", "PRT", "EGY", "PER", "IRN", "ALB", "HKG", "ARG", "BGD", "MAR", "ZMB", "ARE", "DZA", "MMR", "NPL", "VEN",
                     "COD", "KEN", "MOZ", "LBN", "DOM", "NGA", "HND", "TUN", "SEN", "PAN", "JAM", "CYP", "LKA", "ETH", "MUS", "JOR", "CRI", "ISR", 
                     "SGP", "URY", "TZA", "BOL", "HTI", "SYR", "GTM", "SLV")
# Clark2012 countries (66)
u_iso_clark2012 = c("AUS", "AUT", "BEL", "CAN", "DNK", "FIN", "FRA", "DEU", "ISL", "IRL", "ISR", "ITA", "JPN", "LUX", "NLD", "NZL", "NOR", "PRT", "ESP", "SWE", "CHE", "GBR", "USA",
                    "ALB", "DZA", "ARG", "BGD", "BWA", "BRA", "CHL", "CHN", "COL", "CRI", "CYP", "DOM", "EGY", "GRC", "HND", "HUN", "IND", "IDN", "IRN", "JAM", "KEN", "KOR", "MKD",
                    "MYS", "MEX", "MNG", "MAR", "NPL", "PAK", "PER", "PHL", "POL", "ROU", "ZAF", "LKA", "TZA", "THA", "TUR", "URY", "VEN", "VNM", "ZMB", "ZWE")
defineCases  <- function(projName, selectedDepVars, selectedIndepVars, period, treemod, SENSITIVITY_ANALYSIS=FALSE) {
  
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
    for (kdv in selectedDepVars) {
      for (kiv in names(selectedIndepVars)) {
        for (ktm in treemod) {
          if (ktm == "Std-MLinROLS") {
            tmp_demean    <- FALSE
            tmp_firstdiff <- FALSE
          }
          if (ktm == "FE-MLinROLS") {
            tmp_demean    <- TRUE
            tmp_firstdiff <- FALSE
          }
          if (ktm == "FD-MLinROLS") {
            tmp_demean    <- FALSE
            tmp_firstdiff <- TRUE
          }
          
          if (kiv != "Clark2012") {
            tmp_iso <- u_iso #getCountries(period[[kp]][1], c(kdv, selectedIndepVars[[kiv]]))
          } else {
            tmp_iso <- u_iso_clark2012
          }
          
          print(paste0(projName, " - ", kdv, "-", kiv, " - ", ktm, " - ", period[[kp]][1], "-", period[[kp]][2]))
          
          out[[paste0(projName, " - ", kdv, "-", kiv, " - ", ktm, " - ", period[[kp]][1], "-", period[[kp]][2])]] <- 
            list(
              vardef    = generate_variableDefinition(v_dataShort, d=kdv, n=selectedIndepVars[[kiv]][which(selectedIndepVars[[kiv]] != kdv)]),
              period    = period[[kp]],
              iso       = tmp_iso,
              dvar      = kdv,
              demean    = tmp_demean,
              firstdiff = tmp_firstdiff,
              treetype  = "Single tree > LMS - Multilinear",
              maxsplits = 6, minnbnodes = 3,
              cex       = 0.3, legend.cex = 1.5
            )
          
          if (SENSITIVITY_ANALYSIS) {
            for (ky in 0:2) { # Start year
              for (kd in c(5,6,7)) {
                for (kn in c(3,10)) {
                  if (ky != 0 && kd != 6 && ky != 3){
                    print(paste0(projName, " - ", kdv, "-", kiv, " - ", ktm, " - ", period[[kp]][1]+ky, "-", period[[kp]][2], " - md", kd, "-mn", kn))
                    out[[paste0(projName, " - ", kdv, "-", kiv, " - ", ktm, " - ", period[[kp]][1]+ky, "-", period[[kp]][2], " - md", kd, "-mn", kn)]] <- 
                      list(
                        vardef    = generate_variableDefinition(v_dataShort, d=kdv, n=selectedIndepVars[[kiv]][which(selectedIndepVars[[kiv]] != kdv)]),
                        period    = c(period[[kp]][1]+ky, period[[kp]][2]),
                        iso       = tmp_iso,
                        dvar      = kdv,
                        demean    = tmp_demean,
                        firstdiff = tmp_firstdiff,
                        treetype  = "Single tree > LMS - Multilinear",
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
  return(out)
  
}
u_cases <- defineCases(testingMethodName, u_selectedDepVars,u_selectedIndepVars,u_period,u_treeModels, SENSITIVITY_ANALYSIS=FALSE)

#==== Initialise ========================
v_tree  <- list()
v_data  <- list()
v_alloc <- list()
v_allocOriginal <- list()
v_regdata <- list()

v_runtime <- c()

dir.create(file.path("output", testingMethodName, "Plots data"))
dir.create(file.path("output", testingMethodName, "Plots Trees"))
dir.create(file.path("output", testingMethodName, "Plots country pathways"))
dir.create(file.path("output", testingMethodName, "Pred Plots"))
dir.create(file.path("output", testingMethodName, "check_data"), recursive = TRUE)

#==== Run experiments ===================
t1 <- Sys.time()
if (!file.exists(file.path("output", testingMethodName, "data", paste0(testingMethodName, "_GUIDEresults_", format(Sys.time(), "%Y-%m-%d"),".RData")))) {
  t1 <- Sys.time()
  for (k_tm in names(u_cases)[2]) {
    #TODO: make this one interactive on demand
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
    if (!is.null(u_cases[[k_tm]]$vardef)) {
      tmp_vardef <- u_cases[[k_tm]]$vardef
      tmp_vardef$type[which(tmp_vardef$variable %in% c("E_CC", "E_CCGDP", "E_CCP"))] <- "x"
      tmp_vardef$type[which(tmp_vardef$variable == u_cases[[k_tm]]$dvar)] <- "d"
    } else {
      tmp_vardef <- u_variableDefinition
      tmp_vardef$type[which(tmp_vardef$variable %in% c("E_CC", "E_CCGDP", "E_CCP"))] <- "x"
      tmp_vardef$type[which(tmp_vardef$variable == u_cases[[k_tm]]$dvar)] <- "d"
    }
    
    tmp_iso <- u_cases[[k_tm]]$iso
    v_dataShortProc <- data_prepare(v_dataShort %>% filter(iso %in% tmp_iso) %>% spread(variable, value) , 
                                    tmp_vardef, 
                                    u_cases[[k_tm]]$period[1], u_cases[[k_tm]]$period[2], 
                                    MISSINGRATIO = 0.90, REMOVE_ZEROS = FALSE, 
                                    FIRSTDIFF = u_cases[[k_tm]]$firstdiff, 
                                    DEMEAN    = u_cases[[k_tm]]$demean, 
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
    
    v_tree[[k_tm]]          <- list()
    v_data[[k_tm]]          <- list()
    v_alloc[[k_tm]]         <- list()
    v_allocOriginal[[k_tm]] <- list()
    v_regdata[[k_tm]]       <- list()
    
    rmarkdown::render(input  = "regression_diagnostics.Rmd", 
           params = list(data=v_dataShortProc$data),
           output_dir  = file.path("output", testingMethodName, "check_data"),
           output_file = paste0("RD_", k_tm, ".html"))
    detach("package:MASS", unload=TRUE)
    
    # Randomise data
    for (k_case in 0:Ncases) {
      
      t11 <- Sys.time()
      
      k_caseName <- ifelse(nchar(k_case) < nchar(Ncases), paste0(paste(rep("0", nchar(Ncases)-nchar(k_case)), collapse=""), k_case), paste0(k_case))
      
      cat(paste0(k_tm, " - Experiment ", k_caseName , ": "))
      
      # Initialise GUIDE file paths and tree options
      outPath  <- file.path("output", testingMethodName, "RT models", paste0(k_tm, " - T", k_caseName))
      treePath <- initpath(outPath)
      dir.create(treePath$outPath, recursive = TRUE)

      # Sample data (remove observations)
      if (k_case != 0) {
        # Get number of observations in default tree
        Nobs <- dim(v_data[[k_tm]][["0"]]$data)[1]
        
        # Remove 1 observation
        tmp_data_sample <- v_data[[k_tm]][["0"]]
        tmp_data_sample$data <- tmp_data_sample$data %>% sample_n(Nobs-1)
        v_data[[k_tm]][[paste(k_case)]] <- tmp_data_sample
        
        # Remove 1% of observations
        if (k_case > 100) {
          tmp_data_sample <- v_data[[k_tm]][["0"]]
          tmp_data_sample$data <- tmp_data_sample$data %>% sample_frac(0.99)
          v_data[[k_tm]][[paste(k_case)]] <- tmp_data_sample
        }
        
        # Remove 5% of observations
        if (k_case > 200) {
          tmp_data_sample <- v_data[[k_tm]][["0"]]
          tmp_data_sample$data <- tmp_data_sample$data %>% sample_frac(0.95)
          v_data[[k_tm]][[paste(k_case)]] <- tmp_data_sample
        }
        
        # Remove 10% of observations
        if (k_case > 300) {
          tmp_data_sample <- v_data[[k_tm]][["0"]]
          tmp_data_sample$data <- tmp_data_sample$data %>% sample_frac(0.90)
          v_data[[k_tm]][[paste(k_case)]] <- tmp_data_sample
        }
      } else {
        v_data[[k_tm]][[paste(k_case)]] <- v_dataShortProc
      }
      
      
      # render(input  = "regression_diagnostics.Rmd", 
      #        params = list(data=v_data[[k_tm]][[paste(k_case)]]$data),
      #        output_dir = outPath)
      
      # Generate GUIDE input data
      v_dataShortProcGUIDE <- generate_input(v_data[[k_tm]][[paste(k_case)]]$data, v_variableDefinition, 
                                             i_treeType    = u_cases[[k_tm]]$treetype,  # "Single tree > LMS - Multilinear"
                                             i_treeOptions = treeOpts, 
                                             i_fpath       = treePath)
      
      # Run GUIDE
      run_guide(treePath)
      
      v_tree[[k_tm]][[paste(k_case)]]          <- NA
      v_alloc[[k_tm]][[paste(k_case)]]         <- NA
      v_allocOriginal[[k_tm]][[paste(k_case)]] <- NA
      v_regdata[[k_tm]][[paste(k_case)]]       <- NA
      
      
      # Collect and process results
      parseFile <- TRUE
      tryCatch(
        v_tree[[k_tm]][[paste(k_case)]]  <- parse_output(file.path(outPath, "GUIDEfile_out.txt"), v_variableDefinition),
        error = function(e) {
          print("Could not parse output file. Skipping...")
          parseFile <<- FALSE
        }
      )
      
      if (parseFile) {
        v_alloc[[k_tm]][[paste(k_case)]] <- allocateDataToNodes(v_data[[k_tm]][[paste(k_case)]]$data, v_tree[[k_tm]][[paste(k_case)]]) 
        if (!is.null(v_dataShortProc$datamean)) {
          v_allocOriginal[[k_tm]][[paste(k_case)]] <- v_alloc[[k_tm]][[paste(k_case)]] %>% 
            gather(variable, value, -iso,-year,-tnode) %>% 
            mutate(original_variable=gsub("log_","",variable)) %>% 
            left_join( v_dataShortProc$datamean, by=c("iso","original_variable"="variable")) %>% 
            mutate(original_value=exp(value+mean))
        } else {
          v_allocOriginal[[k_tm]][[paste(k_case)]] <- v_alloc[[k_tm]][[paste(k_case)]] %>% 
            gather(variable, value, -iso,-year,-tnode) %>% 
            mutate(original_variable=gsub("log_","",variable)) %>% 
            mutate(original_value=exp(value))
        }
        regfile <- file.path(outPath, "GUIDEfile_regnames.txt")
        
        if (file.exists(regfile)) {
          v_regdata[[k_tm]][[paste(k_case)]] <- parse_regcoef(regfile)
        } 
      }
      
      
      runtime <- Sys.time() - t11
      v_runtime <- c(v_runtime, runtime)
      
      # Save data in folder
      guide_data <- list(
        case  = u_cases[[k_tm]],
        data  = v_data[[k_tm]][[paste(k_case)]], 
        tree  = v_tree[[k_tm]][[paste(k_case)]], 
        alloc = v_alloc[[k_tm]][[paste(k_case)]], 
        alloc_original = v_allocOriginal[[k_tm]][[paste(k_case)]] , 
        regdata = v_regdata[[k_tm]][[paste(k_case)]],
        runtime = runtime
      )
      save(guide_data, file = file.path(outPath, paste0(testingMethodName, "_GUIDEdata_", format(Sys.time(), "%Y-%m-%d"),".RData")))
      
      cat(paste0(" Run time ",  runtime, "(s)\n"))
      
      
      #==== Plot results ==============
      t2 <- Sys.time()
      
      cat(paste0("Plotting Experiment ", k_caseName , "\n"))
      
      if (!is.null(v_tree[[k_tm]][[paste(k_case)]])) {
        # Data
        p = ggplot(data = v_data[[k_tm]][[paste(k_case)]]$data %>% 
                     gather(variable, value, -iso, -year) %>% 
                     filter(
                       iso %in% c("USA", "DEU", "AUS", "IND", "IDN", "ZAF"),
                       variable %in% c("log_E_CC", "log_GDPpc", "log_pop", "log_GDPpc", "log_E_CIm", "log_GDP_Ag", "log_GDP_Ind", "log_GDP_Tra", "log_URB"))) +
          geom_line(aes(x=year, y=value)) +
          facet_grid(variable~iso, scales="free_y") +
          theme_bw()
        #print(p)
        ggsave(p, filename = file.path("output", testingMethodName, "Plots data/", paste0(k_tm ,"_", k_caseName, ".png")), width = 297, height = 210, units = c("mm"), dpi = 300)
        
        # Tree
        tree <- as.fake.rpart.tree(v_tree[[k_tm]][[paste(k_case)]])
        if (dim(tree$frame)[1] != 0) {
          plot_tree(tree, 
                    TITLE=paste0("RT - ", k_case, "\nLMS-Multilinear - Tn:", length(which(v_tree[[k_tm]][[paste(k_case)]]$nodeType == "Terminal node")), "\n#Countries: ",length(unique(v_data[[k_tm]][[paste(k_case)]]$data$iso)), " - period: ",min(v_data[[k_tm]][[paste(k_case)]]$data$year),"-",max(v_data[[k_tm]][[paste(k_case)]]$data$year)), 
                    CEX=NULL, LEGEND.X = 0, LEGEND.Y = 1.1, LEGEND.CEX=0.5, LEGEND.NCOL=2,
                    FILENAME = file.path("output", testingMethodName, "Plots Trees/", paste0(k_tm, "_", k_caseName, ".pdf")))
          plot_tree(tree, 
                    TITLE=paste0("RT - ", k_case, "\nLMS-Multilinear - Tn:", length(which(v_tree[[k_tm]][[paste(k_case)]]$nodeType == "Terminal node")), "\n#Countries: ",length(unique(v_data[[k_tm]][[paste(k_case)]]$data$iso)), " - period: ",min(v_data[[k_tm]][[paste(k_case)]]$data$year),"-",max(v_data[[k_tm]][[paste(k_case)]]$data$year)), 
                    CEX=NULL, LEGEND.X = 0, LEGEND.Y = 1.1, LEGEND.CEX=0.5, LEGEND.NCOL=2,
                    FILENAME = file.path("output", testingMethodName, "Plots Trees/", paste0(k_tm, "_", k_caseName, ".png")))
        }
        
        if (dim(v_alloc[[k_tm]][[paste(k_case)]])[1] != 0) {
          # Country pathways
          png(filename=file.path("output", testingMethodName, "Plots country pathways/", paste0(k_tm, "_", k_caseName, ".png")), width=1500, height=1500, res = 125)
          plot_ISOPathways(c("CHN", "USA"),v_alloc[[k_tm]][[paste(k_case)]], v_tree[[k_tm]][[paste(k_case)]])
          dev.off()
          
          # Prediction plots
          dvar = paste0("log_", u_cases[[k_tm]]$dvar)
          
          pred_data <- list()
          for (kn in v_regdata[[k_tm]][[paste(k_case)]]$Node) {
            tmp_data = v_regdata[[k_tm]][[paste(k_case)]] %>% 
              filter(Node == kn)
            tmp_cst = tmp_data$Constant
            tmp_reg = tmp_data %>% 
              select(-Node, -Constant) %>% 
              gather(variable, regcoeff)
            
            tmp_data <- v_alloc[[k_tm]][[paste(k_case)]] %>% 
              filter(tnode == kn) %>% 
              select(-tnode) %>% 
              gather(variable, value, -iso, -year)
            original <- tmp_data %>% 
              filter(variable == dvar)
            predict <- tmp_data %>% 
              filter(variable != dvar) %>% 
              left_join(tmp_reg, by=c("variable")) %>% 
              #=== reproducing GUIDE approach to fill NAs (i.e. global average)
              group_by(variable) %>% 
              mutate(value = ifelse(is.na(value), mean(value, na.rm=TRUE), value)) %>% 
              ungroup() %>% 
              #=========
            mutate(tmp = value*regcoeff) %>%
              group_by(iso,year) %>% 
              summarize(estimate=sum(tmp, na.rm=TRUE)+tmp_cst) %>% 
              ungroup()
            
            pred_data[[paste(kn)]] <- left_join(original, predict, by=c("iso","year")) %>% 
              mutate(node = kn)
            
          }
          pred_data <- do.call("rbind", pred_data)
          
          p = ggplot(pred_data) +
            geom_segment(aes(x=-1.5, xend=1.5, y=-1.5, yend=1.5), color="red") +
            geom_point(aes(x=value, y=estimate, label=paste0(iso,"-",year))) +
            facet_wrap(~node, ncol=6) +
            xlab("original") + ylab("estimate")
          #print(p)
          ggsave(filename = file.path(paste0("output/", testingMethodName, "/Pred Plots/"), paste0(k_tm, "_", k_caseName, ".png")), width = 297, height = 210, units = c("mm"), dpi = 300)
        }
      }

      plot_exp <- Sys.time() - t2
      cat(paste0("It took ",  plot_exp, "(s) to plot this experiment.\n"))
    
    }
  }
  
  # Save input and results for presentation
  dir.create(file.path("output", testingMethodName, "data"))
  save(u_cases, v_data, v_tree, v_alloc, v_regdata, v_runtime, file = file.path("output", testingMethodName, "data", paste0(testingMethodName, "_GUIDEresults_", format(Sys.time(), "%Y-%m-%d"),".RData")))
  
  runtime_allexp <- Sys.time() - t1
  
  cat(paste0("It took ",  runtime_allexp, "(m) to perform all experiments.\n"))
} else {
  load(file.path("output", testingMethodName, "data", paste0(testingMethodName, "_GUIDEresults_", format(Sys.time(), "%Y-%m-%d"),".RData")))
  load_allexp <- Sys.time() - t1
  cat(paste0("It took ",  load_allexp, "(s) to load all experiments.\n"))
}


#==== Plot results ==============
t2 <- Sys.time()

for (k_case in 1:Ncases) {
  
  k_caseName <- ifelse(nchar(k_case) < nchar(Ncases), paste0(rep("0", nchar(Ncases)-nchar(k_case)), k_case), paste0(k_case))
  
  cat(paste0("Plotting Experiment ", k_caseName , "\n"))
  
  if (!is.null(v_tree[[k_case]])) {
    # Tree
    tree <- as.fake.rpart.tree(v_tree[[k_case]])
    if (dim(tree$frame)[1] != 0) {
      plot_tree(tree, 
                TITLE=paste0("RT - ", k_case, "\nLMS-Multilinear - Tn:", length(which(v_tree[[k_case]]$nodeType == "Terminal node")), "\n#Countries: ",length(unique(v_data[[k_case]]$data$iso)), " - period: ",min(v_data[[k_case]]$data$year),"-",max(v_data[[k_case]]$data$year)), 
                CEX=NULL, LEGEND.X = 0, LEGEND.Y = 1.1, LEGEND.CEX=0.5, LEGEND.NCOL=2,
                FILENAME = file.path("output", testingMethodName, "Plots Trees/", paste0(k_caseName, ".pdf")))
      plot_tree(tree, 
                TITLE=paste0("RT - ", k_case, "\nLMS-Multilinear - Tn:", length(which(v_tree[[k_case]]$nodeType == "Terminal node")), "\n#Countries: ",length(unique(v_data[[k_case]]$data$iso)), " - period: ",min(v_data[[k_case]]$data$year),"-",max(v_data[[k_case]]$data$year)), 
                CEX=NULL, LEGEND.X = 0, LEGEND.Y = 1.1, LEGEND.CEX=0.5, LEGEND.NCOL=2,
                FILENAME = file.path("output", testingMethodName, "Plots Trees/", paste0(k_caseName, ".png")))
    }
    
    if (dim(v_alloc[[k_case]])[1] != 0) {
      # Country pathways
      png(filename=file.path("output", testingMethodName, "Plots country pathways/", paste0(k_caseName, ".png")), width=1500, height=1500)
      plot_ISOPathways(c("CHN", "USA"),v_alloc[[k_case]], v_tree[[k_case]])
      dev.off()
      
      # Prediction plots
      dvar = paste0("log_", u_cases[[1]]$dvar)
      
      pred_data <- list()
      for (kn in v_regdata[[k_case]]$Node) {
        tmp_data = v_regdata[[k_case]] %>% 
          filter(Node == kn)
        tmp_cst = tmp_data$Constant
        tmp_reg = tmp_data %>% 
          select(-Node, -Constant) %>% 
          gather(variable, regcoeff)
        
        tmp_data <- v_alloc[[k_case]] %>% 
          filter(tnode == kn) %>% 
          select(-tnode) %>% 
          gather(variable, value, -iso, -year)
        original <- tmp_data %>% 
          filter(variable == dvar)
        predict <- tmp_data %>% 
          filter(variable != dvar) %>% 
          left_join(tmp_reg, by=c("variable")) %>% 
          #=== reproducing GUIDE approach to fill NAs (i.e. global average)
          group_by(variable) %>% 
          mutate(value = ifelse(is.na(value), mean(value, na.rm=TRUE), value)) %>% 
          ungroup() %>% 
          #=========
        mutate(tmp = value*regcoeff) %>%
          group_by(iso,year) %>% 
          summarize(estimate=sum(tmp, na.rm=TRUE)+tmp_cst) %>% 
          ungroup()
        
        pred_data[[paste(kn)]] <- left_join(original, predict, by=c("iso","year")) %>% 
          mutate(node = kn)
        
      }
      pred_data <- do.call("rbind", pred_data)
      
      p = ggplot(pred_data) +
        geom_segment(aes(x=-1.5, xend=1.5, y=-1.5, yend=1.5), color="red") +
        geom_point(aes(x=value, y=estimate)) +
        facet_wrap(~node, ncol=6) +
        xlab("original") + ylab("estimate")
      #print(p)
      ggsave(filename = file.path(paste0("output/", testingMethodName, "/Pred Plots/"), paste0(k_caseName, ".png")), width = 297, height = 210, units = c("mm"), dpi = 300)
    }
  }
}
plot_allexp <- Sys.time() - t2
cat(paste0("It took ",  plot_allexp, "(s) to plot all experiments.\n"))


p = ggplot(data = v_dataShortProc$data %>% 
             gather(variable, value, -iso, -year) %>% 
             filter(
               iso %in% c("USA", "DEU", "AUS", "IND", "IDN", "ZAF"),
               variable %in% c("log_E_CC", "log_GDPpc", "log_pop", "log_GDPpc", "log_E_CIm", "log_GDP_Ag", "log_GDP_Ind", "log_GDP_Ser" ))) +
  geom_line(aes(x=year, y=value)) +
  facet_grid(variable~iso, scales="free_y") +
  theme_bw()
print(p)


#==== Compare trees ==============
# TODO: Use edit distance instead? (Same for compare data function?)
t3 <- Sys.time()
# Data (country x year)
compare_trees <- function(kt1, kt2) {
  id1 = kt1
  id2 = kt2
  
  iso1  <- unique(v_alloc[[id1]]$iso)
  iso2  <- unique(v_alloc[[id2]]$iso)
  year1 <- unique(v_alloc[[id1]]$year)
  year2 <- unique(v_alloc[[id2]]$year)
  
  isoyear1 <- (v_alloc[[id1]] %>% mutate(isoyear=paste0(iso,"-",year)))$isoyear
  isoyear2 <- (v_alloc[[id2]] %>% mutate(isoyear=paste0(iso,"-",year)))$isoyear
  
  v_alloc1 <- v_alloc[[id1]] %>% mutate(isoyear=paste0(iso,"-",year)) %>% filter(isoyear %in% isoyear2) %>% select(-isoyear)
  v_alloc2 <- v_alloc[[id2]] %>% mutate(isoyear=paste0(iso,"-",year)) %>% filter(isoyear %in% isoyear1) %>% select(-isoyear)
  
  tmp <- v_tree[[id1]] %>% filter(nodeType == "Terminal node") %>% select(nodeID,n) %>% filter(nodeID %in% unique(v_alloc1$tnode)) %>% arrange(desc(n)) %>% mutate(levelsid1 = paste0(nodeID, " (", n, ")"))
  tNodeIDs1 <- tmp$nodeID
  levelsID1 <- tmp$levelsid1
  tmp <- v_tree[[id2]] %>% filter(nodeType == "Terminal node") %>% select(nodeID,n) %>% filter(nodeID %in% unique(v_alloc2$tnode)) %>% arrange(desc(n)) %>% mutate(levelsid2 = paste0(nodeID, " (", n, ")"))
  tNodeIDs2 <- tmp$nodeID
  levelsID2 <- tmp$levelsid2
  
  
  m_abs  <- matrix(NA, nrow = length(tNodeIDs1), ncol = length(tNodeIDs2))
  m_rel1 <- matrix(NA, nrow = length(tNodeIDs1), ncol = length(tNodeIDs2))
  m_rel2 <- matrix(NA, nrow = length(tNodeIDs1), ncol = length(tNodeIDs2))
  for (kr in 1:length(tNodeIDs1)) {
    id_dp1 <- which(v_alloc1$tnode == tNodeIDs1[kr])
    dp1    <- paste0(v_alloc1$iso[id_dp1], "-", v_alloc1$year[id_dp1])
    nbdp1  <- length(dp1)
    alldp2 <- c()
    for (kc in 1:length(tNodeIDs2)) {
      id_dp2 <- which(v_alloc2$tnode == tNodeIDs2[kc])
      dp2    <- paste0(v_alloc2$iso[id_dp2], "-", v_alloc2$year[id_dp2])
      alldp2 <- c(alldp2, dp2)
      nbdp2  <- length(dp2)
      
      m_abs[kr,kc]  <- length(which(dp1 %in% dp2))
      m_rel1[kr,kc] <- length(which(dp1 %in% dp2))/nbdp1
      m_rel2[kr,kc] <- length(which(dp2 %in% dp1))/nbdp2
    } 
    
    missing_dp1 <- dp1[which(!dp1 %in% alldp2)]
    if (length(missing_dp1) != 0) {
      print(tNodeIDs1[kr])
      print(tNodeIDs2[kr])
      print(missing_dp1)
      stop()
    }
    
  }
  
  process_m <- function(m, id1, id2) {
    colnames(m) <- paste(tNodeIDs2)
    m <- round(m, digits = 2) %>% 
      as.data.frame() %>% 
      mutate(TN_T1=tNodeIDs1) %>% 
      gather(TN_T2, value, -TN_T1) %>% 
      left_join(v_tree[[id1]] %>% select(nodeID,n), by=c("TN_T1"="nodeID")) %>% 
      rename(n1=n) %>% 
      mutate(TN_T1=paste0(TN_T1," (", n1, ")")) %>% 
      left_join(v_tree[[id2]] %>% select(nodeID,n), by=c("TN_T2"="nodeID")) %>% 
      rename(n2=n) %>% 
      mutate(TN_T2=paste0(TN_T2," (", n2, ")")) %>% 
      mutate(TN_T1=factor(TN_T1, levels=levelsID1 ,ordered=TRUE)) %>% 
      mutate(TN_T2=factor(TN_T2, levels=levelsID2 ,ordered=TRUE))
  }
  
  
  m_abs <- process_m(m_abs, id1, id2) %>% 
    mutate(type=ifelse(value == 0, "zero", "nonzero"))
  m_rel1 <- process_m(m_rel1, id1, id2)
  m_rel2 <- process_m(m_rel2, id1, id2)
  m_rel <- left_join(
    m_rel1 %>% 
      rename(rel1=value),
    m_rel2 %>% 
      rename(rel2=value) %>% 
      select(-n1,-n2)
  ) %>% 
    mutate(value = n1/(n1+n2)*rel1 + n2/(n1+n2)*rel2) 
  
  misalloc <- round((m_abs %>% group_by(TN_T1) %>% filter(value != max(value)) %>% summarise(sum=sum(value)) %>% ungroup() %>% summarise(sum=sum(sum)))$sum/sum(m_abs$value)*100, digits=1)
  
  ggplot(m_rel) + 
    geom_tile(aes(x=TN_T2, y=TN_T1, fill=value)) + 
    geom_text(aes(x=TN_T2, y=TN_T1, label=value, colour=type), data=m_abs) +
    scale_fill_gradient(low = "#ffffff", high="#ff0000") +
    scale_color_manual(values=c("nonzero"="#000000", zero="#eeeeee")) +
    ylab(paste0(names(u_cases)[id1], " (", length(tNodeIDs1), ")")) + xlab(paste0(names(u_cases)[id2], " (", length(tNodeIDs2), ")")) +
    ggtitle(paste0("Misallocation (%): ", misalloc))
  
  return(misalloc)
}

compare_treeStructure <- function(kt1, kt2) {
  id1 = kt1
  id2 = kt2
  
  tmp        <- v_tree[[id1]] %>% filter(nodeType == "Terminal node") %>% select(nodeID,n) %>% arrange(desc(n)) %>% mutate(levelsid1 = paste0(nodeID, " (", n, ")"))
  tNodeIDs1  <- tmp$nodeID
  levelsID1  <- tmp$levelsid1
  treepaths1 <- lapply(tNodeIDs1, function(x) {
    nodes <- getParentNodeID(x, v_tree[[id1]])[-1]
    out <- v_tree[[id1]] %>% 
      filter(nodeID %in% nodes) %>% 
      group_by(nodeID) %>% 
      filter(row_number() == 1) %>% 
      ungroup() %>% 
      select(variable, value) %>% 
      unite(col = "varval", variable, value, sep = "#")
    return(out$varval)
  })
  comb1 <- unique(unlist(treepaths1))
  
  tmp        <- v_tree[[id2]] %>% filter(nodeType == "Terminal node") %>% select(nodeID,n) %>% arrange(desc(n)) %>% mutate(levelsid2 = paste0(nodeID, " (", n, ")"))
  tNodeIDs2  <- tmp$nodeID
  levelsID2  <- tmp$levelsid2
  treepaths2 <- lapply(tNodeIDs2, function(x) {
    nodes <- getParentNodeID(x, v_tree[[id2]])[-1]
    out <- v_tree[[id2]] %>% 
      filter(nodeID %in% nodes) %>% 
      group_by(nodeID) %>% 
      filter(row_number() == 1) %>% 
      ungroup() %>% 
      select(variable, value) %>% 
      unite(col = "varval", variable, value, sep = "#")
    return(out$varval)
  })
  comb2 <- unique(unlist(treepaths2))
  
  return(length(which(comb1 %in% comb2))/length(comb1)*100)
}

m_comp  <- matrix(NA, nrow = Ncases, ncol = Ncases)
m_compS  <- matrix(NA, nrow = Ncases, ncol = Ncases)

for (kt1 in 1:Ncases) {
  for (kt2 in 1:Ncases) {
    cat(paste0("Comparing T", kt1, " with T", k_t2 , "\n"))
    m_comp[kt1, kt2]  <- compare_trees(kt1, kt2)
    m_compS[kt1, kt2] <- compare_treeStructure(kt1, kt2)
  }
}

save(m_comp, file = file.path("output", testingMethodName, "data", paste0(testingMethodName, "_compData_", format(Sys.time(), "%Y-%m-%d"),".RData")))
save(m_compS, file = file.path("output", testingMethodName, "data", paste0(testingMethodName, "_compTreeStructure_", format(Sys.time(), "%Y-%m-%d"),".RData")))

tmp <- m_comp

colnames(tmp) <- paste(1:Ncases)

namenbtn <- do.call("rbind", lapply(paste(1:Ncases), function(x) data.frame(name=x, nbtn=length(unique(v_alloc[[as.numeric(x)]]$tnode)))))

tmp <- tmp %>% 
  as.data.frame() %>% 
  mutate(T1=paste(1:Ncases)) %>% 
  gather(T2, value, -T1) %>% 
  left_join(namenbtn, by=c("T1"="name")) %>% 
  mutate(T1 = paste0(T1, " (", nbtn, ")")) %>% 
  select(-nbtn) %>% 
  left_join(namenbtn, by=c("T2"="name")) %>% 
  mutate(T2 = paste0(T2, " (", nbtn, ")")) %>% 
  select(-nbtn)

ggplot(tmp) + 
  geom_tile(aes(x=T2, y=T1, fill=value)) + 
  geom_text(aes(x=T2, y=T1, label=value)) +
  scale_fill_gradient(low = "#ffffff", high="#ff0000") +
  theme(axis.text.x = element_text(angle=90))

# Tree structure
tmp <- m_compS

colnames(tmp) <- paste(1:Ncases)

namenbtn <- do.call("rbind", lapply(paste(1:Ncases), function(x) data.frame(name=x, nbtn=length(unique(v_alloc[[as.numeric(x)]]$tnode)))))

tmp <- tmp %>% 
  as.data.frame() %>% 
  mutate(T1=paste(1:Ncases)) %>% 
  gather(T2, value, -T1) %>% 
  left_join(namenbtn, by=c("T1"="name")) %>% 
  mutate(T1 = paste0(T1, " (", nbtn, ")")) %>% 
  select(-nbtn) %>% 
  left_join(namenbtn, by=c("T2"="name")) %>% 
  mutate(T2 = paste0(T2, " (", nbtn, ")")) %>% 
  select(-nbtn)

ggplot(tmp %>% mutate(fmt_val = format(value, digits = 2))) + 
  geom_tile(aes(x=T2, y=T1, fill=value)) + 
  geom_text(aes(x=T2, y=T1, label=fmt_val)) +
  scale_fill_gradient(low = "#ffffff", high="#ff0000") +
  theme(axis.text.x = element_text(angle=90))

comp_allexp <- Sys.time() - t3
cat(paste0("It took ",  comp_allexp, "(s) to compare all experiments.\n"))