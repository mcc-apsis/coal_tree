data_addNewVar <- function(data, formulas, newUnit = "None", newLongname = "Unspecified", newSource = "Own",
                           variable = "variable", 
                           value = "value") {
  
  out <- data

  for (kf in names(formulas)) {
    formula      <- stats::as.formula(formulas[[kf]])
    .dots        <- list(c(as.character(formula[3]), newUnit))
    names(.dots) <- as.character(formula[2])
    
    # guardians
    if (!is.data.frame(data))
      stop("Only works with data frames")
    
    if (!is.list(.dots))
      stop("'.dots' must be a list of formula strings")
    
    
    .colnames <- colnames(data)
    
    if (!variable %in% .colnames)
      stop("No column '", variable, "' found'")
    
    if (!value %in% .colnames)
      stop("No column '", value, "' found'")
    
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
    
    # filter for variables used on rhs
    .data <- out %>%
      select(country, iso, year, variable, value) %>% 
      filter_(.dots = lazyeval::interp(~variable %in% .dots.names,
                                       variable = as.name(variable),
                                       .dots.names = .dots.names))
    
    # calculation
    .data <- .data %>%
      spread_(variable, value) %>%
      mutate_(.dots = .dots) %>%
      gather_(variable, value, unique(c(.dots.names, names(.dots))))
    
    
    # restore longname, source, unit columns
    .data <- .data %>% 
      mutate(longname = newLongname) %>% 
      mutate(source   = newSource) %>% 
      mutate(unit     = newUnit) %>% 
      select(country,iso,year,variable,longname,source,unit,value)
    
    # add unaffected variables
    out <- rbind(
      out %>%
        filter_(.dots = lazyeval::interp(~!variable %in% .dots.names,
                                         variable = as.name(variable),
                                         .dots.names = .dots.names)),
      .data
    )    
  }
  
  

  
  return(out)
}