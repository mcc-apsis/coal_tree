#---- Load libraries ------
library(dplyr)
library(tidyr)
library(ggplot2)

#---- Load functions ------
# Load functions in functions/
dump <- lapply(list.files("functions/", pattern=".R", full.names=TRUE, recursive=TRUE))
rm("dump")
  
#---- Load data -----------
# Load data defined in user_settings.R
load(u_pathData)


