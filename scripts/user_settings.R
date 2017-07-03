u_verbose = TRUE

#---- Files and directories ---------
u_path      <- list()
u_path$data <- "data/coal_data.RData"
u_path$func <- "functions"


#---- Default parameters ------------
u_period       <- c(1970, 2012)
u_iso          <- c("AUS", "AUT", "BEL", "CAN", "DNK", "FIN", "FRA", "DEU", "ISL", "IRL", "ISR", "ITA", "JPN", "LUX", "NLD", "NZL", "NOR", "PRT", "ESP", "SWE", "CHE", "GBR", "USA",
                    "ALB", "DZA", "ARG", "BGD", "BWA", "BRA", "CHL", "CHN", "COL", "CRI", "CYP", "DOM", "EGY", "GRC", "HND", "HUN", "IND", "IDN", "IRN", "JAM", "KEN", "KOR", "MKD",
                    "MYS", "MEX", "MNG", "MAR", "NPL", "PAK", "PER", "PHL", "POL", "ROU", "ZAF", "LKA", "TZA", "THA", "TUR", "URY", "VEN", "VNM", "ZMB", "ZWE")
u_perCap_vars  <- c("GDP", "E_TotFeC")
u_squared_vars <- c("GDP", "GDPpc")
u_d1_vars      <- c("GDP", "GDPpc")
u_d2_vars      <- c("GDP", "GDPpc")
u_dummy_var    <- c("E_CE")
u_aggreg_var   <- list(
  "E_RenC" = c("E_BC", "E_HC", "E_RC", "E_GeoC"))
u_compute_var  <- list(
  "E_CC/GDP"  = c("E_CCGDP~E_CC/GDP"),
  "E_CP/GDP"  = c("E_CPGDP~E_CP/GDP"),
  "E_CIm/GDP" = c("E_CImGDP~E_CIm/GDP"),
  "E_CEx/GDP" = c("E_CExGDP~E_CEx/GDP"),
  "E_CC/P"    = c("E_CCP~E_CC/P")
)

#---- Variable selection ------------
u_select_var <- function(i_data) {
  out <- i_data %>% 
  filter(variable %in% c("E_CC", "E_CCbc", "E_CChc", "E_CCli", "E_CP", "E_CIm", "E_CEx", "E_CE", "Elec_C", "E_CRe", 
                         "E_CRes", "E_CSeInd",
                         "E_OC", "E_GC", "E_NC", "E_HC", "E_RC", "E_GeoC", "E_BC",
                         "Elec",
                         "E_TotP", "E_TotPeS", "E_TotFeC", "EE", "EF",
                         "P", "P_Ndep", 
                         "GDP", 
                         "GDP_Ag", "GDP_Ind", "GDP_Ser", "GDP_Tra",
                         "K", "Lab", "Debt_Ext",
                         "Manu", "Manu_Ex", "Steel", "Constr",
                         "GINI", "Inst", "LEx", "URB", "PDen",
                         "CO2",
                         "AP_pm25mae", "AP_pm25pe")) %>% 
  filter(!(variable == "E_CC"   & source != "IEA2016 - WEB")) %>% 
  filter(!(variable == "E_CChc" & source != "IEA2016 - WEB")) %>%
  filter(!(variable == "E_CCbc" & source != "IEA2016 - WEB")) %>%
  filter(!(variable == "E_CCli" & source != "IEA2016 - WEB")) %>%
  filter(!(variable == "E_OC"   & source != "IEA2016 - WEB")) %>% 
  filter(!(variable == "E_GC"   & source != "IEA2016 - WEB")) %>% 
  filter(!(variable == "E_NC"   & source != "IEA2016 - WEB")) %>% 
  filter(!(variable == "E_HC"   & source != "IEA2016 - WEB")) %>% 
  filter(!(variable == "E_RC"   & source != "IEA2016 - WEB")) %>% 
  filter(!(variable == "E_BC"   & source != "IEA2016 - WEB")) %>% 
  filter(!(variable == "E_CP"   & source != "IEA2016 - WEB")) %>%
  filter(!(variable == "E_CIm"  & source != "IEA2016 - WEB")) %>%
  filter(!(variable == "E_CEx"  & source != "IEA2016 - WEB")) %>%
  filter(!(variable == "E_CRes" & source != "IEA2016 - WEB")) %>%
  filter(!(variable == "E_CInd" & source != "IEA2016 - WEB")) %>%
  filter(!(variable == "E_CE"   & source != "BP 2016")) %>%
  filter(!(variable == "Elec_C" & source != "IEA2016 - WEB")) %>%
  filter(!(variable == "EE"     & source != "IEA2016 - WEB + Own calculation")) %>%
  filter(!(variable == "EF"     & source != "IEA2016 - WEB + Own calculation")) %>%
  filter(!(variable == "GDP"    & source != "Penn World Tables")) %>%
  filter(!(variable == "GINI"   & source != "SWIID")) %>%
  filter(!(variable == "CO2"    & source != "CDIAC" & longname == "CO2 emissions")) %>% 
  filter(!(variable == "P"      & source != "UN")) 
  
  return(out)
}

#---- Variable Definition -----------
#TODO: make this one interactive on demand
u_variableDefinition <- list(
  #---- (d) dependent variable (choose one) -----------------------------
  "E_CC"       = c(type="d", factor=1,     transform="log", demean=TRUE,  firstdiff=FALSE),   # Coal consumption
  #---- (t) observation time --------------------------------------------
  "year"       = c(type="x", factor=1,     transform="log", demean=TRUE,  firstdiff=FALSE), 
  #---- (c) categorical variable used for splitting only ----------------
  "E_CE"       = c(type="x", factor=1,     transform="",    demean=FALSE, firstdiff=FALSE),   # Coal endowment
  #---- (r) categorical treatment variable used for fitting only --------
  #---- (n) numerical variable used for both splitting and fitting ------
  "E_CChc"     = c(type="x", factor=1,     transform="log", demean=TRUE,  firstdiff=FALSE),   # Coal consumption HC
  "E_CCbc"     = c(type="x", factor=1,     transform="log", demean=TRUE,  firstdiff=FALSE),   # Coal consumption BC
  "E_CCli"     = c(type="n", factor=1,     transform="log", demean=TRUE,  firstdiff=FALSE),   # Coal consumption Lignite
  "E_CP"       = c(type="n", factor=1,     transform="log", demean=TRUE,  firstdiff=FALSE),   # Coal production
  "E_CIm"      = c(type="n", factor=1,     transform="log", demean=TRUE,  firstdiff=FALSE),   # Coal Imports
  "E_CEx"      = c(type="n", factor=1,     transform="log", demean=TRUE,  firstdiff=FALSE),   # Coal exports
  "E_CRe"      = c(type="x", factor=1,     transform="log", demean=TRUE,  firstdiff=FALSE),   # Coal rents
  "Elec_C"     = c(type="n", factor=1,     transform="log", demean=TRUE,  firstdiff=FALSE),   # Electricity production from coal
  "E_CRes"     = c(type="n", factor=1,     transform="log", demean=TRUE,  firstdiff=FALSE),   # Coal use in residencial sector
  "E_CSeInd"   = c(type="n", factor=1,     transform="log", demean=TRUE,  firstdiff=FALSE),   # Coal use in industry sector
  "Elec"       = c(type="n", factor=1,     transform="log", demean=TRUE,  firstdiff=FALSE),   # Electricity production
  "E_OC"       = c(type="n", factor=1,     transform="log", demean=TRUE,  firstdiff=FALSE),   # Oil consumption 
  "E_GC"       = c(type="n", factor=1,     transform="log", demean=TRUE,  firstdiff=FALSE),   # Gas consumption
  "E_NC"       = c(type="n", factor=1,     transform="log", demean=TRUE,  firstdiff=FALSE),   # Nuclear consumption
  "E_HC"       = c(type="x", factor=1,     transform="log", demean=TRUE,  firstdiff=FALSE),   # Hydropower consumption
  "E_RC"       = c(type="x", factor=1,     transform="log", demean=TRUE,  firstdiff=FALSE),   # Renewables consumption
  "E_GeoC"     = c(type="x", factor=1,     transform="log", demean=TRUE,  firstdiff=FALSE),   # Geothermal consumption
  "E_BC"       = c(type="x", factor=1,     transform="log", demean=TRUE,  firstdiff=FALSE),   # Biomass consumption
  "E_TotP"     = c(type="x", factor=1,     transform="log", demean=TRUE,  firstdiff=FALSE),   # Total P?
  "E_TotPeS"   = c(type="x", factor=1,     transform="log", demean=TRUE,  firstdiff=FALSE),   # Total Primary Energy Supply
  "E_TotFeC"   = c(type="x", factor=1,     transform="log", demean=TRUE,  firstdiff=FALSE),   # Total Final Energy Consumption
  "CO2"        = c(type="x", factor=1,     transform="log", demean=TRUE,  firstdiff=FALSE),   # CO2 emissions
  "AP_pm25mae" = c(type="x", factor=1,     transform="log", demean=TRUE,  firstdiff=FALSE),   # PM2.5 
  "AP_pm25pe"  = c(type="x", factor=1,     transform="log", demean=TRUE,  firstdiff=FALSE),   # PM2.5 
  "P"          = c(type="n", factor=1,     transform="log", demean=TRUE,  firstdiff=FALSE),   # Population
  "P_Ndep"     = c(type="n", factor=1,     transform="log", demean=TRUE,  firstdiff=FALSE),   # Population (24-65 years old)
  "GDP"        = c(type="n", factor=1,     transform="log", demean=TRUE,  firstdiff=FALSE),   # GDP
  "GDP^2"      = c(type="x", factor=1,     transform="log", demean=TRUE,  firstdiff=FALSE),   # Square of GDP to detect EKC
  "d1_GDP"     = c(type="x", factor=1,     transform="",    demean=TRUE,  firstdiff=FALSE),   # TODO: Use definition from Hausman
  "d2_GDP"     = c(type="x", factor=1,     transform="",    demean=TRUE,  firstdiff=FALSE),   # TODO: Use definition from Hausman
  "GDPpc"      = c(type="n", factor=1,     transform="log", demean=TRUE,  firstdiff=FALSE),   # GDP per capita
  "GDPpc^2"    = c(type="x", factor=1,     transform="log", demean=TRUE,  firstdiff=FALSE),   # Square of GDP to detect EKC
  "d1_GDPpc"   = c(type="x", factor=1,     transform="",    demean=TRUE,  firstdiff=FALSE),   # TODO: Use definition from Hausman
  "d2_GDPpc"   = c(type="x", factor=1,     transform="",    demean=TRUE,  firstdiff=FALSE),   # TODO: Use definition from Hausman
  "K"          = c(type="x", factor=1,     transform="log", demean=TRUE,  firstdiff=FALSE),   # Gross Capital formation
  "Lab"        = c(type="x", factor=1,     transform="log", demean=TRUE,  firstdiff=FALSE),   # Labour
  "Debt_Ext"   = c(type="x", factor=1,     transform="log", demean=TRUE,  firstdiff=FALSE),   # External debt
  "LEx"        = c(type="n", factor=1,     transform="log", demean=TRUE,  firstdiff=FALSE),   # Life expectancy      
  "GINI"       = c(type="x", factor=1,     transform="log", demean=TRUE,  firstdiff=FALSE),   # Inequality (GINI coefficient)
  "URB"        = c(type="n", factor=1,     transform="log", demean=TRUE,  firstdiff=FALSE),   # Urban population ratio
  "PDen"       = c(type="x", factor=1e-3,  transform="log", demean=TRUE,  firstdiff=FALSE),   # Population density
  "GDP_Ag"     = c(type="n", factor=1,     transform="log", demean=TRUE,  firstdiff=FALSE),   # Sectoral share of GDP (Agriculture)
  "GDP_Ind"    = c(type="n", factor=1,     transform="log", demean=TRUE,  firstdiff=FALSE),   # Sectoral share of GDP (Industry)
  "GDP_Ser"    = c(type="n", factor=1,     transform="log", demean=TRUE,  firstdiff=FALSE),   # Sectoral share of GDP (Services)
  "GDP_Tra"    = c(type="n", factor=1,     transform="log", demean=TRUE,  firstdiff=FALSE),   # Sectoral share of GDP (Trade)
  "Manu"       = c(type="x", factor=1,     transform="log", demean=TRUE,  firstdiff=FALSE),   # Sectoral share of GDP (Trade)
  "Manu_Ex"    = c(type="x", factor=1,     transform="log", demean=TRUE,  firstdiff=FALSE),   # Sectoral share of GDP (Trade)
  "Inst"       = c(type="x", factor=1,     transform="log", demean=TRUE,  firstdiff=FALSE),   # Institutional quality index
  "EE"         = c(type="n", factor=1,     transform="log", demean=TRUE,  firstdiff=FALSE),   # Energy Efficiency FE/PE
  "EF"         = c(type="x", factor=1,     transform="log", demean=TRUE,  firstdiff=FALSE),   # Emission intensity Emi/FE
  #---- (f) numerical variable used for fitting only --------------------
  #---- (s) numerical variable used for splitting only ------------------
  #---- other variables -------------------------------------------------
  "iso"        = c(type="x", factor=NA,    transform="",    demean=FALSE, firstdiff=FALSE))