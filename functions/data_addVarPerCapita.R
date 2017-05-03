data_addVarPerCapita <- function(i_data, i_vars) {
  
  out <- i_data
  
  pop <- i_data %>% 
    filter(variable == "P") %>% 
    select(iso, year, variable, value) %>% 
    spread(variable, value)
  
  for (kv in i_vars) {
    out <- out %>% 
      rbind(left_join(
        out %>% 
          filter(variable == kv),
        pop) %>% 
          mutate(variable=paste0(variable, "pc")) %>% 
          mutate(longname=paste0(longname, "per capita")) %>% 
          mutate(source="Own") %>% 
          mutate(unit=paste0(unit, "/Cap")) %>% 
          mutate(value=value/P) %>% 
          select(-P))
  }
  rm("pop")
  
  return(out)
  
}