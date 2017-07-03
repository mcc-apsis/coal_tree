tm_plot_modelFittingPerformance <- function(i_case, i_regdata, i_alloc, i_filename) {
  
  # Get dependent variable name
  dvar <- ifelse(i_case$log == "TRUE", paste0("log_", i_case$dvar), i_case$dvar)
  
  pred_data <- list()
  
  # Loop over nodes
  for (kn in i_regdata$Node) {
    tmp_data <- i_regdata %>% 
      filter(Node == kn)
    
    tmp_cst = tmp_data$Constant

    tmp_reg = tmp_data %>% 
      select(-Node, -Constant) %>% 
      gather(variable, regcoeff)
    
    tmp_data <- i_alloc %>% 
      filter(tnode == kn) %>% 
      select(-tnode) %>% 
      gather(variable, value, -iso, -year)

    original <- tmp_data %>% 
      filter(variable == dvar)

    predict <- tmp_data %>% 
      filter(variable != dvar) %>% 
      left_join(tmp_reg, by=c("variable")) %>% 
      #=== reproducing GUIDE approach to fill NAs (i.e. global average)
      group_by(variable) %>% 
      mutate(value = ifelse(is.na(value), mean(value, na.rm=TRUE), value)) %>% 
      ungroup() %>% 
      #=========
      mutate(tmp = value*regcoeff) %>%
      group_by(iso,year) %>% 
      summarize(estimate=sum(tmp, na.rm=TRUE)+tmp_cst) %>% 
      ungroup()
    
    print(head(predict))
    
    pred_data[[paste(kn)]] <- left_join(original, predict, by=c("iso","year")) %>% 
      mutate(node = kn)
    
  }
  pred_data <- do.call("rbind", pred_data)
  
  # Plot fitting performance
  p = ggplot(pred_data) +
    geom_segment(aes(x=-1.5, xend=1.5, y=-1.5, yend=1.5), color="red") +
    geom_point(aes(x=value, y=estimate, label=paste0(iso,"-",year))) +
    facet_wrap(~node, ncol=6) +
    xlab("original") + ylab("estimate")
  
  ggsave(filename = i_filename, width = 297, height = 210, units = c("mm"), dpi = 300)
}