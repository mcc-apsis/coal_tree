# Demeaning function compatible with dplyr
demean <- function(i_data, i_var) {
  
  # Save zeros
  id_zeros <- which(i_data[,i_var] == 0)
  i_data[id_zeros,i_var] = NA
    
  # Compute mean
  mutate_call <- lazyeval::interp(~ mean(i_var, na.rm=TRUE), i_var = as.name(i_var))
  i_data <- i_data %>% 
    mutate_(.dots = setNames(list(mutate_call), "varmean"))
  
  # Demean
  mutate_call <- lazyeval::interp(~ ifelse(!is.na(i_var), i_var - varmean, NA), i_var = as.name(i_var))
  i_data <- i_data %>% mutate_(.dots = setNames(list(mutate_call), i_var))
  
  # Remove variable mean
  i_data <- i_data %>% 
    select(-varmean)
  
  # Restore zeros
  i_data[id_zeros,i_var] = 0.0
  
  return(i_data)
}