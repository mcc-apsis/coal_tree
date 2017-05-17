generate_variableDefinition <- function (
  data,
  d=NULL,n=NULL,f=NULL,s=NULL,c=NULL,r=NULL,x=NULL,
  transform = "log",
  demean    = TRUE,
  firstdiff = FALSE
) {
  
  vars <- unique(data$variable)
  myvars <- c(d,n,f,s,c)
  test <- myvars[which(! myvars %in% vars)]
  
  if (length(test) != 0) {
    cat(paste("Error: The following variable(s) is(are) not in the data:", paste(test, collpase=","), ".\n"))
    stop()
  }
  
  out <- data.frame()
  
  # Dependent variable
  if (length(d) == 1) {
    out <- out %>% 
      rbind(data.frame(
        variable = d,
        type = "d",
        factor = 1,
        transform=transform,
        demean=demean,
        firstdiff=firstdiff
      ))
  } else {
    stop()
  }
  
  # Numerical variables: n, f, s
  if (length(n) != 0) {
    for (kn in n) {
      out <- out %>% 
        rbind(data.frame(
          variable = kn,
          type = "n",
          factor = 1,
          transform=transform,
          demean=demean,
          firstdiff=firstdiff
        ))  
    }
  }
  if (length(f) != 0) {
    for (kf in f) {
      out <- out %>% 
        rbind(data.frame(
          variable = kf,
          type = "f",
          factor = 1,
          transform=transform,
          demean=demean,
          firstdiff=firstdiff
        ))  
    }
  }
  if (length(s) != 0) {
    for (ks in s) {
      out <- out %>% 
        rbind(data.frame(
          variable = ks,
          type = "s",
          factor = 1,
          transform=transform,
          demean=demean,
          firstdiff=firstdiff
        ))  
    }
  }
  
  # Categorical variables: c
  if (length(c) != 0) {
    for (kc in c) {
      out <- out %>% 
        rbind(data.frame(
          variable = kc,
          type = "c",
          factor = 1,
          transform="",
          demean=FALSE,
          firstdiff=FALSE
        ))  
    }
  }
  
  # Excluded variables: x
  x <- vars[which(!vars %in% c(d,n,f,s,c))]
  for (kx in x) {
    out <- out %>% 
      rbind(data.frame(
        variable = kx,
        type = "x",
        factor = 1,
        transform="",
        demean=FALSE,
        firstdiff=FALSE
      ))  
  }
  
  # year and iso 
  out <- out %>% 
    rbind(data.frame(
      variable = "year",
      type = "x",
      factor = 1,
      transform="",
      demean=FALSE,
      firstdiff=FALSE
    ),
    data.frame(
      variable = "iso",
      type = "x",
      factor = 1,
      transform="",
      demean=FALSE,
      firstdiff=FALSE
    ))
  
  out <- out %>% 
    mutate(variable = paste(variable))
  
  return(out)
}