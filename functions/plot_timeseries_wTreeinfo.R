plot_timeseries_wTreeinfo <- function(i_data, i_tree, i_alloc, i_iso, i_var, i_dvar="log_E_CC", file=NULL, ...) {
  
  tmp_data = i_data %>% 
    gather(variable, value, -iso, -year) %>% 
    filter(
      iso %in% i_iso,
      variable %in% c(i_dvar, i_var))
  
  tmp_tree  = i_tree
  tmp_alloc = i_alloc
  
  
  tmp_cPathway <- do.call("rbind", lapply(i_iso, function(x) {
    tryCatch(df <- getCountryPathway(tmp_alloc, x) %>% 
               mutate(iso = x) %>% 
               select(iso, start, end, node),
             error = function(e) {
                 print(paste0("Warning: Country ", x, " is not in i_alloc and will be dropped."))
                 return(data.frame())
               })
             }))
  
  p = ggplot(data = tmp_data)
  
  # 0 line
  p = p + 
    geom_segment(aes(x=min(tmp_data$year), xend=max(tmp_data$year), y=0, yend=0), 
                 data=NULL,
                 col="black")
  
  # Terminal nodes
  p = p + geom_rect(
    aes(xmin=start, xmax=end, ymin=minval, ymax=maxval, fill=type),
    data=tmp_data %>% 
      group_by(iso, variable) %>% 
      summarise(minval = min(value, na.rm=TRUE), 
                maxval = max(value, na.rm=TRUE)) %>% 
      left_join(tmp_cPathway, by=c("iso")) %>% 
      group_by(iso) %>% 
      mutate(type = row_number() %% 2 == 1)
  ) +
    geom_label(aes(x=midpoint, y=max(maxval), label=node),
               size = 2,
               data=tmp_data %>% 
                 group_by(iso, variable) %>% 
                 summarise(minval = min(value, na.rm=TRUE), 
                           maxval = max(value, na.rm=TRUE)) %>% 
                 left_join(tmp_cPathway, by=c("iso")) %>% 
                 group_by(iso) %>% 
                 filter(variable == i_dvar) %>% 
                 mutate(midpoint = (start+end)/2) %>% 
                 mutate(type = row_number() %% 2 == 1))
  
  # Intermediate nodes
  p = p + 
    geom_segment(aes(x=ymin, xend=ymax, y=value, yend=value), 
                 data=tmp_tree %>% 
                   filter(nodeType != "Terminal node", operator %in% c("<=")) %>% 
                   select(variable, value) %>% 
                   filter(variable %in% i_var) %>% 
                   mutate(ymin = min(tmp_data$year)) %>% 
                   mutate(ymax = max(tmp_data$year)),
                 col="red", lty=3)
  p = p +
    geom_line(aes(x=year, y=value), col="blue", lwd=1.25) +
    facet_grid(variable~iso, scales="free_y") +
    xlab("") + ylab("value") +
    scale_fill_manual(values=c("TRUE"="#99999966", "FALSE"="#eeeeee66")) +
    theme_bw()
  print(p)
  
  if (!is.null(file)) ggsave(p, filename = file, ...)
  
  return(p)
  
}