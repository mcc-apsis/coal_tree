u_mode = "R"

source("scripts/user_settings.R")
source("scripts/init.R")

# Initialise GUIDE file paths and tree options
treePath <- initpath("output/test")
treeOpts <- initTreeOptions()  #TODO: make this one interactive on demand

# Generate GUIDE input data
v_dataShortProcGUIDE <- generate_input(v_dataShortProc, v_variableDefinition, 
                                       #i_treeType    = "Single tree > LMS - Best simple linear",
                                       i_treeType    = "Single tree > LMS - Multilinear", 
                                       i_treeOptions = treeOpts, 
                                       i_fpath       = treePath)


# Run GUIDE
t0 <- Sys.time()
run(treePath)
print(Sys.time()-t0)

# Collect and process results
results <- parse_output("output/test/GUIDEfile_out.txt")
v_alloc <- allocateDataToNodes(v_dataShortProc, results)

# Plot tree
tree <- as.fake.rpart.tree(results)
plot_tree(tree, 
          TITLE=paste0("RT\n#Countries: ",length(unique(v_dataShortProc$iso)), " - period: ",min(v_dataShortProc$year),"-",max(v_dataShortProc$year), "\nLMS-Multilinear - Tn:", length(which(results$nodeType == "Terminal node"))), 
          CEX=0.1, LEGEND.X = 0, LEGEND.Y = 1.1, LEGEND.CEX=2.9, LEGEND.NCOL=2)

# Plot scatter iso
plot_ScatterCountry(v_alloc, results, "DEU" ,"log_P", "log_E_CC", "log_E_CC", PLOT_LABELS = TRUE)
plot_ScatterCountry_allVars(v_alloc, results, c("DEU", "CHN"), "log_E_CC", PLOT_LABELS = TRUE)

# Plot Tnodes
for (kn in as.numeric(results$nodeID[which(results$nodeType == "Terminal node")])) {
  tryCatch(
    plot_TNode(v_alloc, kn, results, PLOT_TRANSITION = FALSE),
    error = function(e) {
      cat(paste0("Error with node: ", kn, "\n"))
    }
  )
}

