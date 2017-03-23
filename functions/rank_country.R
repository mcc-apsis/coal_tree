rank_country <- function(i_data, i_function, i_variable, i_source, i_period=c(1960, 2014)) {
  
  out <- i_data %>% 
    filter(variable == i_variable, source == i_source, year >= i_period[1], year <= i_period[2]) %>% 
    select(iso, year, value) %>% 
    group_by(iso) %>% 
    summarize(
      min  = min(value, na.rm=TRUE),
      mean = mean(value, na.rm=TRUE),
      max  = max(value, na.rm=TRUE)) %>% 
    ungroup
  
  if (i_function == "max") {
    out <- out %>% 
      arrange(desc(max)) %>% 
      select(iso)
  }
  
  if (i_function == "mean") {
    out <- out %>% 
      arrange(desc(mean)) %>% 
      select(iso)
  }
  
  if (i_function == "min") {
    out <- out %>% 
      arrange(desc(min)) %>% 
      select(iso)
  }
  
  return(paste(out$iso))
}