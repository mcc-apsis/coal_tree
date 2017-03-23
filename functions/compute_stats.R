compute_stats <- function(i_data, digits=3) {
  out <- i_data %>% 
    select(iso, year, variable, value) %>% 
    group_by(variable) %>% 
    summarize(
      min    = format(min(value, na.rm=TRUE), scientific=TRUE, digits=digits), 
      p05    = format(quantile(value, 0.05, na.rm=TRUE), scientific=TRUE, digits=digits),
      p25    = format(quantile(value, 0.25, na.rm=TRUE), scientific=TRUE, digits=digits), 
      median = format(median(value, na.rm=TRUE), scientific=TRUE, digits=digits), 
      mean   = format(mean(value, na.rm=TRUE), scientific=TRUE, digits=digits),
      p75    = format(quantile(value, 0.75, na.rm=TRUE), scientific=TRUE, digits=digits), 
      p95    = format(quantile(value, 0.95, na.rm=TRUE), scientific=TRUE, digits=digits),
      max    = format(max(value, na.rm=TRUE), scientific=TRUE, digits=digits),
      NAs    = length(which(is.na(value)))) %>% 
    ungroup()
    
    return(out)
}