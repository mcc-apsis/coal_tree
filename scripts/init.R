if (u_mode == "knitr") u_path <- lapply(u_path, function(x) file.path("..", x))
if (u_mode == "shinypres") u_verbose = FALSE

#---- Load libraries ------
suppressMessages(library(RColorBrewer))
suppressMessages(library(readxl))
suppressMessages(library(guidr))
suppressMessages(library(igraph))
suppressMessages(library(dplyr))
suppressMessages(library(tidyr))
suppressMessages(library(magrittr))
suppressMessages(library(ggplot2))
suppressMessages(library(plotly))
suppressMessages(library(scatterpie))
if (as.logical(Sys.info()["sysname"] == "Linux"))   suppressMessages(library(doMC))        # Linux library to distribute tasks to CPUs
if (as.logical(Sys.info()["sysname"] == "Windows")) suppressMessages(library(doSNOW))      # Windows library to distribute tasks to CPUs
suppressMessages(library(foreach))      # Library to enable parallelisation of tasks
if (u_mode == "knitr") {
  suppressMessages(library(corrplot))
  suppressMessages(library(GGally))
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
