data_aggregateVar <- function(i_data, i_vars, VERBOSE=TRUE) {
  out <- i_data
  
  for (kv in names(i_vars)) {
    tmp1 <- out %>% 
      filter(!variable %in% i_vars[[kv]])
    tmp2 <- out %>% 
      filter(variable %in% i_vars[[kv]])
    
    if (length(unique(tmp2$unit)) != 1 && VERBOSE) cat("Warning: units are not consistent across the selected variables.\n")
    
    tmp_unit = tmp2$unit[1]
    
    tmp2 <- tmp2 %>% 
              select(country,iso,year,variable,value) %>% 
              group_by(country,iso,year) %>% 
              summarize(value=sum(value,na.rm=TRUE)) %>% 
              ungroup() %>% 
              mutate(variable=kv) %>% 
              mutate(longname=paste0("Aggregated variable: ", kv, "=", paste(i_vars[[kv]], collapse="+"))) %>% 
              mutate(source="Own") %>% 
              mutate(unit=tmp_unit) %>% 
              select(country, iso, year, variable, longname, source, unit, value)


    out <- rbind(tmp1, tmp2)
  }
  
  return(out)
}