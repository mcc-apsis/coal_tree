#---- Files and directories ---------
u_pathData = "data/coal_data.RData"

#---- Variable selection ------------
u_select_var <- function(i_data) {
  out <- i_data %>% 
  filter(variable %in% c("E_CC", "E_CP", "E_CIm", "E_CEx", "E_CE", "Elec_C", "E_CRe", 
                       "E_OC", "E_GC", "E_NC", "E_HC", "E_RC", "E_BC",
                       "EnPri", "EnFin", "EE", "EF",
                       "P", "P_Ndep",
                       "GDP", "GINI", "Inst", "LEx", "URB", "PDen",
                       "GDP_Ag", "GDP_Ind", "GDP_Ser", "GDP_Tra",
                       "Manu", "Manu_Ex",
                       "CO2",
                       "AP_pm25mae", "AP_pm25pe")) %>% 
  filter(!(variable == "E_CC"   & source != "IEA2016 - WEB")) %>% 
  filter(!(variable == "E_OC"   & source != "IEA2016 - WEB")) %>% 
  filter(!(variable == "E_GC"   & source != "IEA2016 - WEB")) %>% 
  filter(!(variable == "E_NC"   & source != "IEA2016 - WEB")) %>% 
  filter(!(variable == "E_HC"   & source != "IEA2016 - WEB")) %>% 
  filter(!(variable == "E_RC"   & source != "IEA2016 - WEB")) %>% 
  filter(!(variable == "E_BC"   & source != "IEA2016 - WEB")) %>% 
  filter(!(variable == "E_CP"   & source != "IEA2016 - WEB")) %>%
  filter(!(variable == "E_CIm"  & source != "IEA2016 - WEB")) %>%
  filter(!(variable == "E_CEx"  & source != "IEA2016 - WEB")) %>%
  filter(!(variable == "E_CE"   & source != "BP2016")) %>%
  filter(!(variable == "Elec_C" & source != "IEA2016 - WEB")) %>%
  filter(!(variable == "GDP"    & source != "Penn World Tables")) %>%
  filter(!(variable == "GINI"   & source != "SWIID")) %>%
  filter(!(variable == "CO2"    & source != "CDIAC" & longname == "CO2 emissions")) %>%
  select(iso, year, variable, value)
  return(out)
}