data_addVarSquared <- function(i_data, i_vars) {

  out <- i_data
  
  for (kv in i_vars) {
    out <- out %>% 
      rbind(out %>% 
              filter(variable == kv) %>% 
              mutate(variable=paste0(variable, "^2")) %>% 
              mutate(longname=paste0("Squared ", longname)) %>% 
              mutate(source="Own") %>% 
              mutate(unit=paste0("(",unit,")^2")) %>% 
              mutate(value=value^2))
  }
  
  return(out)
}