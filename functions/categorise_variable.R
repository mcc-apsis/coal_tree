categorise_variable <- function(i_data, i_var, i_ncat=2, i_threshold=NULL, i_normvar=NULL) {
  
  out <- i_data
  
  # Cumulate values
  out <- out %>% 
    filter(variable == c(i_var, i_normvar)) %>% 
    group_by(iso, variable) %>% 
    summarise(value = sum(value, na.rm=TRUE)) %>% 
    ungroup() %>% 
    spread(variable, value)
  
  # Normalise data
  if (!is.null(i_normvar)) {
    out$value <- out[[i_var]]/out[[i_normvar]]
  } else {
    out$value <- out[[i_var]]
  }
  
  # Get rid of NAs and Infinite values
  out <- out %>% filter(!is.na(value)) %>% filter(is.finite(value))
  print(head(out))
  
  # Categorise data
  if (is.null(i_threshold)) {
    if (i_ncat == 2) {
      out <- out %>% 
        mutate(category = ifelse(value < 0.2*max(value, na.rm=TRUE), "low", "high"))
    } else {
      for (k in 1:(i_ncat-1)) {
        out <- out %>% mutate(category = ifelse(value < 0.2*max(value, na.rm=TRUE), paste0("C", k), paste0("C", i_ncat)))
      }
    }
  } else {
    for (k in 1:(i_ncat-1)) {
      out <- out %>% mutate(category = ifelse(value <= i_threshold[k], paste0("C", k), paste0("C", i_ncat)))
    }
    
  }
  
  return(out)
}