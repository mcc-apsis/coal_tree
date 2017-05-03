#---- Variable selection (by sources) -----
v_dataLong <- u_select_var(p_data)

#---- Filter our years and iso -----
# v_dataLong <- v_dataLong %>% 
#   filter(
#     iso %in% u_iso,
#     year >= u_period[1],
#     year <= u_period[2]
#   )

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
v_dataLong           <- data_aggregateVar(v_dataLong, u_aggreg_var)
u_variableDefinition <- data_aggregateVarDef(u_variableDefinition, u_aggreg_var)

#---- Add new variables ---------------------------------------
v_dataLong           <- data_addNewVar(v_dataLong, u_compute_var)
u_variableDefinition <- data_addNewVarDef(u_variableDefinition, u_compute_var)

#---- Create short version of the dataset ---------------------
v_dataShort <- v_dataLong %>%
  select(iso, year, variable, value)


#---- Save variable names -------------------------------------
v_variables <- unique(v_dataLong$variable)