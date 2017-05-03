data_addNewVarDef <- function(varDef, formulas, newUnit = "None", newLongname = "Unspecified", newSource = "Own",
                           variable = "variable", 
                           value = "value") {
  
  out <- varDef
  
  for (kf in names(formulas)) {
    formula      <- stats::as.formula(formulas[[kf]])
    .dots        <- list(c(as.character(formula[3]), newUnit))
    names(.dots) <- as.character(formula[2])
    .dots <- lapply(.dots,
                    function(l) {
                      paste0("~", l[[1]]) %>%
                        stats::formula() %>%
                        lazyeval::interp()
                    })
    names(.dots) <- gsub("`", "", names(.dots))
    .dots.names <- lapply(.dots, all.vars) %>%
      unlist() %>%
      unique() %>%
      setdiff(names(.dots))
    
    tmp1 <- out %>% 
      filter(!variable %in% .dots.names)
    tmp2 <- out %>% 
      filter(variable %in% .dots.names)

    if (length(unique(tmp2$factor)) != 1) {
      cat("Warning: variable factor is not consistent across the selected variables.\n")
    }
    tmp_factor = tmp2$factor[1]
    
    if (length(unique(tmp2$type)) != 1) {
      cat("Warning: variable type is not consistent across the selected variables.\n")
    }
    tmp_type = "n"
    
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
    
    tmp3 <- data.frame(
      variable  = names(.dots),
      type      = tmp_type,
      factor    = tmp_factor,
      transform = tmp_transform,
      demean    = tmp_demean,
      firstdiff = tmp_firstdiff 
    )
    
    tmp2 <- tmp2 %>% 
      mutate(type = "x")
    
    out <- rbind(tmp1, tmp2, tmp3)
    
  }
  
  return(out) 
}