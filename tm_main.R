#==== USER SECTION ================
u_doParallel = TRUE
u_mode   = "R"
source("scripts/user_settings.R")

u_processData = FALSE
u_rerun       = TRUE

u_smoothenData = TRUE

#---- Testing methods options -----
testingMethodName <- "testingMethods10" 
Ncases            <- 3
u_remove1obs      <- TRUE

#---- Defining cases --------------
u_period = list(
  #"LongTerm20y" = c(1991, 2012),
  "LongTerm40y" = c(1971, 2012)
  )
u_selectedDepVars = c("E_CC", "E_CCGDP")

v_catVars <- c("E_CE")

u_selectedIndepVars = list(
  # "Clark2012"        = c("GDPpc", "P", "P_Ndep", "URB", "Manu", "Manu_Ex"),
  # "EnhancedSet"      = c("E_CIm", "E_CEx", "E_CP", 
  #                        "GDPpc", "P", "P_Ndep", 
  #                        "URB", "LEx", "EE", 
  #                        "GDP_Ag", "GDP_Ind", "GDP_Ser", "GDP_Tra"),
  # "EnhancedSetNoSer" = c("E_CIm", "E_CEx", "E_CP", 
  #                        "GDPpc", "P", "P_Ndep", 
  #                        "URB", "LEx", "EE", 
  #                        "GDP_Ag", "GDP_Ind", "GDP_Tra"),
  # "EnhancedSetNoAgr" = c("E_CIm", "E_CEx", "E_CP", 
  #                        "GDPpc", "P", "P_Ndep", 
  #                        "URB", "LEx", "EE", 
  #                        "GDP_Ser", "GDP_Ind", "GDP_Tra"),
  "SimpleSet"        = c("E_CP", "GDPpc", "P", "EE", "GDP_Ag", "GDP_Ind", "GDP_Ser", "GDP_Tra"))
u_treeModels = c("FE-ConROLS", "FE-LinROLS", "FE-MLinROLS")
# 80 countries
u_iso = c("CHN","USA", "DEU", "IND", "JPN", "POL", "GBR", "ZAF", "FRA", "KOR", "TUR", "CAN", "AUS", "BEL", "ITA", "BRA", "ESP", "IDN",
          "VNM", "THA", "HUN", "ROU", "AUT", "BGR", "NLD", "PAK", "MEX", "COL", "SWE", "FIN", "ZWE", "NOR", "NZL", "DNK", "GRC", "PHL", 
          "MYS", "CHL", "CHE", "PRT", "EGY", "PER", "IRN", "ALB", "HKG", "ARG", "BGD", "MAR", "ZMB", "ARE", "DZA", "MMR", "NPL", "VEN",
          "COD", "KEN", "MOZ", "LBN", "DOM", "NGA", "HND", "TUN", "SEN", "PAN", "JAM", "CYP", "LKA", "ETH", "MUS", "JOR", "CRI", "ISR", 
          "SGP", "URY", "TZA", "BOL", "HTI", "SYR", "GTM", "SLV")
# Clark2012 countries (66)
u_iso_clark2012 = c("AUS", "AUT", "BEL", "CAN", "DNK", "FIN", "FRA", "DEU", "ISL", "IRL", "ISR", "ITA", "JPN", "LUX", "NLD", "NZL", "NOR", "PRT", "ESP", "SWE", "CHE", "GBR", "USA",
                    "ALB", "DZA", "ARG", "BGD", "BWA", "BRA", "CHL", "CHN", "COL", "CRI", "CYP", "DOM", "EGY", "GRC", "HND", "HUN", "IND", "IDN", "IRN", "JAM", "KEN", "KOR", "MKD",
                    "MYS", "MEX", "MNG", "MAR", "NPL", "PAK", "PER", "PHL", "POL", "ROU", "ZAF", "LKA", "TZA", "THA", "TUR", "URY", "VEN", "VNM", "ZMB", "ZWE")
u_isos <- list(
  #"C20" = u_iso[1:20],
  #"C40" = u_iso[1:40],
  "C80" = u_iso[1:80]
)

u_missingRatio <- c(0.90)

#---- Plotting options -------------
u_timeseriesTreeInfoPlots      = TRUE
u_treePlots                    = TRUE
u_countryPathwaysPlots         = TRUE
u_modelFittingPerformancePlots = TRUE


#==== INITIALISE ===========
source("tm_init.R")


#==== RUN EXPERIMENTS ======
source("tm_run.R")

#==== ANALYSE RESULTS ======
#source("tm_results.R")




#==== CLEAN UP ======
if (u_doParallel) {
  if (Sys.info()["sysname"] == "Windows") stopCluster(cl)
}