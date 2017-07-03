tm_prepare_data <- function(i_case, i_data) {
  
  if (!is.null(i_case$vardef)) {
    tmp_vardef <- i_case$vardef
    tmp_vardef$type[which(tmp_vardef$variable %in% c("E_CC", "E_CCGDP", "E_CCP"))] <- "x"
    tmp_vardef$type[which(tmp_vardef$variable == i_case$dvar)] <- "d"
  } else {
    stop()
  }

  tmp_iso <- i_case$iso
  
  # Smoothen data
  if (u_smoothenData) {
    tmp_data <- i_data  %>% 
      filter(iso %in% tmp_iso) %>% 
      spread(variable, value)
    
    tmp_vars <- names(tmp_data)[which(!names(tmp_data) %in% c("iso","year",paste(v_variableDefinition$variable[which(v_variableDefinition$type != "x")])))]
    
    for (kiso in tmp_iso) {
      print(kiso)
      for (kvar in tmp_vars) {
        print(kvar)        
        m <- loess(y~x, data=data.frame(x=tmp_data$year[which(tmp_data$iso == kiso)], y=tmp_data[[kvar]][which(tmp_data$iso == kiso)]), span=0.75)
        
        tmp_data[which(tmp_data$iso == kiso), kvar] <- predict(m, tmp_data$year[which(tmp_data$iso == kiso)])
        
      }
    }

  } else {
    tmp_data <- i_data  %>% 
      filter(iso %in% tmp_iso) %>% 
      spread(variable, value)
  }
  
  v_dataShortProc <- data_prepare(tmp_data, 
                                  tmp_vardef, 
                                  i_case$period[1], i_case$period[2], 
                                  MISSINGRATIO = 0.90, REMOVE_ZEROS = FALSE, 
                                  FIRSTDIFF = i_case$firstdiff, 
                                  DEMEAN    = i_case$demean, 
                                  VERBOSE   = FALSE)
  
  v_dataShortProc$data <- v_dataShortProc$data %>% 
    gather(variable, value, -iso, -year) %>% 
    mutate(value=ifelse(!is.finite(value), NA, value)) %>% 
    spread(variable, value)
  
  # Redefine variable definition table
  v_variableDefinition <- tmp_vardef 
  v_variableDefinition$variable <- factor(v_variableDefinition$variable, 
                                          levels=paste(get_renamedVariables(tmp_vardef, ALL=TRUE)$old), 
                                          labels=paste(get_renamedVariables(tmp_vardef, ALL=TRUE)$new))
  
  return(list(data=v_dataShortProc, vardef=v_variableDefinition))
}
