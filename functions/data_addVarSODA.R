data_addVarSODA <- function(i_data, i_vars) {
  
  out <- i_data
  
  for (kv in i_vars) {
    
    if (!paste0("d1_", kv) %in% unique(out$variable)) stop()
    
    out <- out %>% 
      rbind(out %>% 
              filter(variable == paste0("d1_", kv)) %>% 
              group_by(iso) %>% 
              arrange(year) %>% 
              mutate(value = (value-lag(value))/(year-lag(year))) %>% 
              ungroup() %>% 
              mutate(variable=paste0("d2_", kv)) %>% 
              mutate(longname=paste0("Second-order derivative approcimation of ", longname)) %>% 
              mutate(source="Own") %>% 
              mutate(unit=paste0("d2/dt2(",unit,")")))
  }
  
  return(out)
}