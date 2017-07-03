tm_parse_results <- function(i_file, i_vardef, i_data) {
  
  out <- list(tree=NA, alloc=NA, regdata=NA)
  
  parseFile <- TRUE
  tryCatch(
    out$tree  <- parse_output(file.path(outPath, "GUIDEfile_out.txt"), v_variableDefinition),
    error = function(e) {
      print("Could not parse output file. Skipping...")
      parseFile <<- FALSE
    }
  )
  
  if (parseFile) {
    out$pode     <- get_pode(file.path(outPath, "GUIDEfile_out.txt"))
    out$secbest  <- get_secondBestSplit(file.path(outPath, "GUIDEfile_out.txt"))
    
    out$alloc <- allocateDataToNodes(i_data$data, out$tree) 
    # if (!is.null(v_dataShortProc$datamean)) {
    #   v_allocOriginal[[k_tm]][[paste(k_case)]] <- v_alloc[[k_tm]][[paste(k_case)]] %>% 
    #     gather(variable, value, -iso,-year,-tnode) %>% 
    #     mutate(original_variable=gsub("log_","",variable)) %>% 
    #     left_join( v_dataShortProc$datamean, by=c("iso","original_variable"="variable")) %>% 
    #     mutate(original_value=exp(value+mean))
    # } else {
    #   v_allocOriginal[[k_tm]][[paste(k_case)]] <- v_alloc[[k_tm]][[paste(k_case)]] %>% 
    #     gather(variable, value, -iso,-year,-tnode) %>% 
    #     mutate(original_variable=gsub("log_","",variable)) %>% 
    #     mutate(original_value=exp(value))
    # }
    out$alloc 
    
    regfile <- file.path(outPath, "GUIDEfile_regnames.txt")
    if (file.exists(regfile) && file.size(file.path(outPath, "GUIDEfile_regnames.txt")) != 0) {
      out$regdata <- parse_regcoef(regfile)
    } else {
      out$regdata <- NULL
    }
  }
  
  return(out)
}

