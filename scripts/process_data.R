#---- Variable selection (by sources) -----
v_dataLong <- u_select_var(p_data)

#-- Remove countries with less than 1 million inhabitants
v_isoPop <- v_dataLong %>% 
  filter(variable == "P") %>% 
  select(iso,year,variable,value) %>% 
  group_by(iso) %>% 
  mutate(value = value*1e-6) %>% 
  summarise(
    min  = min(value, na.rm=TRUE),
    mean = mean(value, na.rm=TRUE),
    max  = max(value, na.rm=TRUE)) %>% 
  ungroup()
v_isoPop <- (v_isoPop %>% filter(max > 1))$iso
v_dataLong <- v_dataLong %>% 
  filter(iso %in% v_isoPop)

#---- Compute EE and EF ------
# v_dataLong <- calcAddVariable(v_dataLong, formula = "EE=E_ToTFeC/E_ToTPeS", unit="Unitless")
# v_dataLong <- calcAddVariable(v_dataLong, formula = "EF=CO2/E_ToTPeS", unit="Mt(CO2)/GJ")

#---- Make Coal exports (E_CEx) a positive number -------
v_dataLong <- v_dataLong %>% 
  mutate(value = ifelse(variable == "E_CEx", -value, value))

#---- Add per-capita variables -------
v_dataLong <- data_addVarPerCapita(v_dataLong, u_perCap_vars)

#---- Add squared variables -------
v_dataLong <- data_addVarSquared(v_dataLong, u_squared_vars)

#---- Add First-order derivate approximation variables --------
v_dataLong <- data_addVarFODA(v_dataLong, u_d1_vars)

#---- Add Second-order derivate approximation variables -------
v_dataLong <- data_addVarSODA(v_dataLong, u_d2_vars)

#---- Transform dummy variables to categorical variables ------
v_dataLong <- data_treatDummyVar(v_dataLong, u_dummy_var)

#---- Aggregate variables -------------------------------------
v_dataLong           <- data_aggregateVar(v_dataLong, u_aggreg_var, VERBOSE=u_verbose)
u_variableDefinition <- data_aggregateVarDef(u_variableDefinition, u_aggreg_var)
u_variableDefinition$type[which(u_variableDefinition$variable == "E_RenC")] <- "n"

#---- Add new variables ---------------------------------------
v_dataLong           <- data_addNewVar(v_dataLong, u_compute_var)
u_variableDefinition <- data_addNewVarDef(u_variableDefinition, u_compute_var, VERBOSE=u_verbose)

#---- Add a few categorical variables -------------------------
# Income group classification by World Bank
isoclass <- read_xls("data/WorldBank/CLASS.xls", skip = 4)[c(-1),c("Code", "Income group")]
# Democracy index by CSP
polity   <- read_xls("data/CSP/p4v2015d_iso.xls")[,c("iso", "country", "polity", "bmonth", "bday", "byear", "emonth", "eday", "eyear")] %>% 
  filter(iso != "", !is.na(iso))
tmp_polity <- lapply(unique(paste0(polity$iso, "-", polity$byear, "-", polity$bmonth, "-", polity$bday)), function(x) {
  tmp_iso    = strsplit(x, "-")[[1]][1]
  tmp_byear  = as.numeric(strsplit(x, "-")[[1]][2])
  tmp_bmonth = as.numeric(strsplit(x, "-")[[1]][3])
  tmp_bday   = as.numeric(strsplit(x, "-")[[1]][4])
  
  tmp = polity[which(polity$iso == tmp_iso & polity$byear == tmp_byear & polity$bmonth == tmp_bmonth & polity$bday == tmp_bday),]
  
  tmp_eyear  = as.numeric(tmp$eyear)
  tmp_emonth = as.numeric(tmp$emonth)
  tmp_eday   = as.numeric(tmp$eday)
  
  tmp_polity = tmp$polity
  
  if(as.numeric(tmp_bmonth) < 7 && as.numeric(tmp_bday) < 15) tmp_byear <- tmp_byear -1
  if(as.numeric(tmp_emonth) < 7 && as.numeric(tmp_eday) < 15) tmp_eyear <- tmp_eyear -1
  
  nyears <- length(tmp_byear:tmp_eyear)
  
  out <- data.frame(
    iso  = rep(tmp_iso, nyears),
    year = tmp_byear:tmp_eyear,
    polity = rep(tmp_polity, nyears)
  )
  return(out)
})
polity = do.call("rbind", tmp_polity)

tmp_var_info1 <- v_dataLong %>% select(country, iso) %>% distinct()
tmp_var_info2 <- v_dataLong %>% select(variable, longname, source, unit) %>% distinct()

v_dataLong <- v_dataLong %>% 
  select(iso, year, variable, value) %>% 
  spread(variable, value) %>% 
  left_join(isoclass, by=c("iso"="Code")) %>% 
  rename(dev=`Income group`) %>% 
  mutate(dev=factor(dev, 
                    levels=c("Low income", "Upper middle income", "High income", "Lower middle income", "NA"), 
                    labels=c("LI", "UMI", "HI", "LMI", "NA"))) %>% 
  left_join(polity, by=c("iso", "year")) %>% 
  rename(pol=polity) %>% 
  gather(variable, value, -iso, -year) %>% 
  left_join(tmp_var_info1) %>% 
  left_join(tmp_var_info2) %>% 
  select(country,iso,year,variable,longname,source,unit,value) %>% 
  mutate(longname = ifelse(variable == "dev", "Income group in 2015", longname)) %>% 
  mutate(source   = ifelse(variable == "dev", "World Bank", longname)) %>% 
  mutate(unit     = ifelse(variable == "dev", "None", longname)) %>% 
  mutate(longname = ifelse(variable == "pol", "Country regime index", longname)) %>% 
  mutate(source   = ifelse(variable == "pol", "CSP-PolityIV", longname)) %>% 
  mutate(unit     = ifelse(variable == "pol", "None", longname))

u_variableDefinition <- u_variableDefinition %>% 
  add_row(variable = "dev", type = "x", factor = NA, transform = "", demean = FALSE, firstdiff = FALSE) %>% 
  add_row(variable = "pol", type = "x", factor = NA, transform = "", demean = FALSE, firstdiff = FALSE)

#---- Create short version of the dataset ---------------------
v_dataShort <- v_dataLong %>%
  select(iso, year, variable, value)


#---- Save variable names -------------------------------------
v_variables <- unique(v_dataLong$variable)