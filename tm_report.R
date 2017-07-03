dir.create(file.path("output", testingMethodName, "reports"))

data_report <- list()

for (k_tm in names(u_cases)) {
  id                <- which(v_tree[[k_tm]][["0"]]$nodeID == 1)[1]

  data_report[[k_tm]] <- data.frame(
    tree_id           = gsub(" ", "", gsub(paste0(testingMethodName, " - "), "", k_tm)),
    dvar              = strsplit(gsub(" ", "", gsub(paste0(testingMethodName, " - "), "", k_tm)), "-", fixed=TRUE)[[1]][1],
    idvarset          = strsplit(gsub(" ", "", gsub(paste0(testingMethodName, " - "), "", k_tm)), "-", fixed=TRUE)[[1]][2],
    treemod           = paste(strsplit(gsub(" ", "", gsub(paste0(testingMethodName, " - "), "", k_tm)), "-", fixed=TRUE)[[1]][c(3,4)], collapse="-"),
    period            = paste(strsplit(gsub(" ", "", gsub(paste0(testingMethodName, " - "), "", k_tm)), "-", fixed=TRUE)[[1]][c(5,6)], collapse="-"),
    first_split       = paste(v_tree[[k_tm]][["0"]]$variable[id], v_tree[[k_tm]][["0"]]$operator[id], v_tree[[k_tm]][["0"]]$value[id]),
    second_best_split = get_secondBestSplit(file.path("output", testingMethodName, "RT models", paste0(k_tm, " - T0"), "GUIDEfile_out.txt")),
    pode              = round(get_pode(file.path("output", testingMethodName, "RT models", paste0(k_tm, " - T0"), "GUIDEfile_out.txt"))*100, digits=2),
    nb_tnodes         = length(which(v_tree[[k_tm]][["0"]]$nodeType == "Terminal node")),
    node_nbobs        = paste0(
      v_tree[[k_tm]][["0"]]$nodeID[which(v_tree[[k_tm]][["0"]]$nodeType == "Terminal node")][which(v_tree[[k_tm]][["0"]]$n[which(v_tree[[k_tm]][["0"]]$nodeType == "Terminal node")]/v_tree[[k_tm]][["0"]]$n[1] == max(v_tree[[k_tm]][["0"]]$n[which(v_tree[[k_tm]][["0"]]$nodeType == "Terminal node")]/v_tree[[k_tm]][["0"]]$n[1]))][1],
      " (", round(max(v_tree[[k_tm]][["0"]]$n[c(which(v_tree[[k_tm]][["0"]]$nodeType == "Terminal node"))]/v_tree[[k_tm]][["0"]]$n[1])*100, digit=2), "%)"
    )
  )
}

data_report <- do.call("rbind", data_report)
row.names(data_report) <- NULL

rmarkdown::render(input  = "tree_summary_statistics.Rmd",
       params = list(data=data_report),
       output_dir  = file.path("output", testingMethodName, "reports"),
       output_file = "Tree Summary Statistics.html")
