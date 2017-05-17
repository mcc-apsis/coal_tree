if (u_mode == "knitr") u_path <- lapply(u_path, function(x) file.path("..", x))

#---- Load libraries ------
library(RColorBrewer)
library(guidr)
library(igraph)
library(dplyr)
library(tidyr)
library(magrittr)
library(ggplot2)
library(scatterpie)
if (u_mode == "knitr") {
  library(corrplot)
  library(GGally)
}

#---- Load functions ------
# Load functions in functions/
dump <- lapply(list.files(u_path$func, pattern=".R", full.names=TRUE, recursive=TRUE), source)
rm("dump")

#---- Convert variable definition ----
# Transform variable definition list to data.frame
u_variableDefinition <- transformListToDataframe(u_variableDefinition) %>% 
  mutate(factor    = as.numeric(paste(factor))) %>% 
  mutate(demean    = as.logical(paste(demean))) %>% 
  mutate(firstdiff = as.logical(paste(firstdiff)))
u_variableDefinition$variable <- gsub(".", "^", u_variableDefinition$variable, fixed = TRUE)

# Dependent variable
v_varDep <- paste(u_variableDefinition$variable[which(u_variableDefinition$type=="d")])
  
#---- Load data -----------
# Load data defined in user_settings.R (object name: p_data)
load(u_path$data)
