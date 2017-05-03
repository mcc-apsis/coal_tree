# Function to transform the list containing the variable definitions into a data frame
transformListToDataframe <- function(i_list) {
  tmp <- t(as.data.frame(i_list))
  df  <- tmp
  row.names(df) <- NULL
  out <- cbind(data.frame(variable=row.names(tmp)), df)
  return(out)
}