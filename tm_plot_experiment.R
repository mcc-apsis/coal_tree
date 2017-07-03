tm_plot_experiment <- function(i_ktm, i_casename, i_tree, i_data, i_alloc, i_regdata, i_pode, i_secbest, i_case, i_iso = c("USA", "CHN", "DEU", "AUS", "IND", "IDN", "ZAF")) {
  
  if (!is.null(i_tree)) {
    
    # Time series and nodes plot
    if (u_timeseriesTreeInfoPlots) {
      isos = i_iso
      vars = c(paste0("log_", i_case$dvar), unique(i_tree$variable[which(i_tree$nodeType != "Terminal node")]))
      
      tryCatch(plot_timeseries_wTreeinfo(i_data$data, 
                                         i_tree, 
                                         i_alloc,
                                         isos,
                                         vars,
                                         i_dvar=paste0("log_", i_case$dvar),
                                         file=file.path("output", testingMethodName, "Plots data/", paste0(i_ktm ,"_", i_casename, ".png")), 
                                         width = 297, height = 210, units = c("mm"), dpi = 300),
               error=function(e) {print("Couldn't run plot_timeseries_wTreeinfo")})
    }
    
    # Tree plot
    if (u_treePlots) {
      
      # Generate fake rpart tree
      tree <- as.fake.rpart.tree(i_tree)
      

      # If successful, generate the tree plot...
      if (dim(tree$frame)[1] != 0) {
        file.remove(file.path("output", testingMethodName, "Plots Trees/", paste0(i_ktm, "_", i_casename, ".png")))
        plot_tree(tree, 
                  TITLE=paste0("RT - ", k_case, " - PoDE: ", i_pode, " - SecBestVsplit: ", i_secbest,  "\nLMS-Multilinear - Tn:", length(which(i_tree$nodeType == "Terminal node")), 
                               "\n#Countries: ",length(unique(i_data$data$iso)), " - period: ",min(i_data$data$year),"-",max(i_data$data$year), 
                               ifelse(u_remove1obs && k_case != 0, paste0("\n Missing obs: ", miss_obs), "")), 
                  CEX=NULL, LEGEND.X = 0, LEGEND.Y = 1.1, LEGEND.CEX=0.5, LEGEND.NCOL=2,
                  FILENAME = file.path("output", testingMethodName, "Plots Trees/", paste0(i_ktm, "_", i_casename, ".png")))
      } else {
        print("Couldn't plot tree")
      }
    }
    
    
    if (dim(i_alloc)[1] != 0) {
      
      # Country pathways plot
      if (u_countryPathwaysPlots) {
        png(filename=file.path("output", testingMethodName, "Plots country pathways/", paste0(i_ktm, "_", i_casename, ".png")), width=2500, height=2500, res = 150)
        par(mfrow=c(3,3))
        for (kiso in c(isos, "COL", "VNM")) {
          plot_ISOPathways(kiso, i_alloc, i_tree)
        }
        par(mfrow=c(1,1))
        dev.off()
      }
      
      # Model fitting performance plots
      if (u_modelFittingPerformancePlots & !is.null(i_regdata)) {
        tm_plot_modelFittingPerformance(
          i_case, 
          i_regdata, 
          i_alloc, 
          file.path(paste0("output/", testingMethodName, "/Plots fitting performance/"), paste0(i_ktm, "_", i_casename, ".png"))
        )
      }
    }
  }
}