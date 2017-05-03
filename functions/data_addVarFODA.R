data_addVarFODA <- function(i_data, i_vars) {
  
  out <- i_data
  
  for (kv in i_vars) {
    out <- out %>% 
      rbind(out %>% 
              filter(variable == kv) %>% 
              group_by(iso) %>% 
              arrange(year) %>% 
              mutate(value = (value-lag(value))/(year-lag(year))) %>% 
              ungroup() %>% 
              mutate(variable=paste0("d1_", variable)) %>% 
              mutate(longname=paste0("First-order derivative approcimation of ", longname)) %>% 
              mutate(source="Own") %>% 
              mutate(unit=paste0("d/dt(",unit,")")))
  }
  
  return(out)
}