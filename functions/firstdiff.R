# First difference function compatible with dplyr
firstdiff <- function(i_data, i_var) {
  
  # First differences
  mutate_call = lazyeval::interp(~ i_var - lag(i_var), i_var = as.name(i_var))
  i_data <- i_data %>% 
    mutate_(.dots = setNames(list(mutate_call), i_var))
  
  return(i_data)
}