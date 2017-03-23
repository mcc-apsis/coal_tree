if (u_mode == "knitr") u_path <- lapply(u_path, function(x) file.path("..", x))

#---- Load libraries ------
library(dplyr)
library(tidyr)
library(ggplot2)
if (u_mode == "knitr") {
  library(corrplot)
  library(GGally)
}

#---- Load functions ------
# Load functions in functions/
dump <- lapply(list.files(u_path$func, pattern=".R", full.names=TRUE, recursive=TRUE), source)
rm("dump")
  
#---- Load data -----------
# Load data defined in user_settings.R
load(u_path$data)

#---- Process data --------
v_dataLong <- u_select_var(p_data)

#---- Add per-capita variables -------
pop <- v_dataLong %>% 
  filter(variable == "P") %>% 
  select(iso, year, variable, value) %>% 
  spread(variable, value)

for (kv in u_perCap_vars) {
  v_dataLong <- v_dataLong %>% 
    rbind(left_join(
    v_dataLong %>% 
      filter(variable == kv),
      pop) %>% 
    mutate(variable=paste0(variable, "pc")) %>% 
    mutate(longname=paste0(longname, "per capita")) %>% 
    mutate(source="Own") %>% 
    mutate(unit=paste0(unit, "/Cap")) %>% 
    mutate(value=value/P) %>% 
    select(-P))
}
rm("pop")

v_dataShort <- v_dataLong %>%
  select(iso, year, variable, value)
v_variables <- v_dataLong$variable[which(duplicated(v_dataLong$variable) == FALSE)]

#---- Log variables --------
v_dataLongProc  <- v_dataLong
v_dataShortProc <- v_dataShort
v_dataLongProc$value <- log(v_dataLongProc$value)
v_dataLongProc$value[which(!is.finite(v_dataLongProc$value))] <- NA
v_dataLongProc$value[which(is.nan(v_dataLongProc$value))]    <- NA
v_dataLongProc$variable <- paste0("log_", v_dataLongProc$variable)

v_dataShortProc <- v_dataLongProc %>%
  select(iso, year, variable, value)
v_variablesProc <- v_dataLongProc$variable[which(duplicated(v_dataLongProc$variable) == FALSE)]