data_aggregateVarDef <- function(i_dataDef, i_vars) {
  out <- i_dataDef
  
  for (kv in names(i_vars)) {
    tmp1 <- out %>% 
      filter(!variable %in% i_vars[[kv]])
    tmp2 <- out %>% 
      filter(variable %in% i_vars[[kv]])

    if (dim(tmp2)[1] != 0) {
    
      if (length(unique(tmp2$factor)) != 1) {
        cat("Error: variable factor is not consistent across the selected variables.\n")
        stop()
      }
      tmp_factor = tmp2$factor[1]
          
      if (length(unique(tmp2$type)) != 1) {
        cat("Error: variable type is not consistent across the selected variables.\n")
        stop()
      }
      tmp_type = tmp2$type[1]
      
      if (length(unique(tmp2$transform)) != 1) {
        cat("Error: variable transformation is not consistent across the selected variables.\n")
        stop()
      }
      tmp_transform = tmp2$transform[1]
      
      if (length(unique(tmp2$demean)) != 1) {
        cat("Error: variable transformation is not consistent across the selected variables.\n")
        stop()
      }
      tmp_demean = tmp2$demean[1]
      
      if (length(unique(tmp2$firstdiff)) != 1) {
        cat("Error: variable transformation is not consistent across the selected variables.\n")
        stop()
      }
      tmp_firstdiff = tmp2$firstdiff[1]
      
      tmp2 <- data.frame(
        variable  = kv,
        type      = tmp_type,
        factor    = tmp_factor,
        transform = tmp_transform,
        demean    = tmp_demean,
        firstdiff = tmp_firstdiff 
      )

      out <- rbind(tmp1, tmp2)
    }
  }
  
  return(out)
}