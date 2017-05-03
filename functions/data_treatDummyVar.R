data_treatDummyVar <- function(i_data, i_vars) {
  
  out <- i_data
  
  for (kv in i_vars) {
  
    v_year     <- i_data$year[which(i_data$variable == kv & !is.na(i_data$value))[1]]
    v_longname <- i_data$longname[which(i_data$variable == kv & !is.na(i_data$value))[1]]
    v_unit     <- i_data$unit[which(i_data$variable == kv & !is.na(i_data$value))[1]]
    v_source   <- i_data$source[which(i_data$variable == kv & !is.na(i_data$value))[1]]
    
    tmp <- left_join(
      left_join(
        expand.grid(list(iso=paste(sort(unique(i_data$iso))), year=as.numeric(paste(sort(unique(i_data$year))))), stringsAsFactors = FALSE),
        i_data %>% 
          select(iso, country) %>% 
          filter(!duplicated(iso)),
        by=c("iso")),
      out %>% 
        filter(variable == kv, year == v_year) %>% 
        select(-year, -country),
      by=c("iso")) %>% 
      mutate(variable = kv) %>% 
      mutate(longname = v_longname) %>% 
      mutate(unit     = v_unit)
      
    tmp$source[which(is.na(tmp$value))] = "Own"
    tmp$value[which(is.na(tmp$value))]  = 0.0
    
    out <- rbind(
      out %>% 
        filter(variable != kv),
      tmp
      )
  }
  
  return(out)
}