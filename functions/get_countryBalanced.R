get_countryBalanced <- function(i_data, i_variable, i_source, i_period=c(1960, 2014)) {
  
  out <- i_data %>% 
    filter(variable == i_variable, source == i_source, year >= i_period[1], year <= i_period[2]) %>% 
    select(iso, year, value)
  
  out$value[is.na(out$value)] <- 0
  
  out <- out %>% 
    group_by(iso) %>% 
    summarize(test = all(value != 0)) %>% 
    ungroup() %>% 
    filter(test == TRUE)
  
  return(paste(out$iso))
}