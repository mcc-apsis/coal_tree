data_demeanVariable <- function(i_data, i_vardef, GROUPBY="country", VERBOSE=FALSE) {
  # Get names of numerical variables
  numVarnames <- paste((i_vardef %>% filter(type %in% c("d", "n", "f", "s")))$variable)
  
  # Loop over variables and demean
  for (kvar in numVarnames) {
    if (VERBOSE) cat("    - ", kvar, "<-", kvar, "- mean(", kvar, ")\n")
    i_data <- i_data %>% 
      group_by_(GROUPBY) %>% 
      demean(kvar) %>% 
      ungroup()
  }
  
  return(i_data)
}