#=============================================
# Script to compile coal data
#
# !! WARNING !! Make sure that the file is 
#               encoded in ISO-8859-1.
#=============================================

#---------------------------------------------
# USER SECTION
#---------------------------------------------
DEBUG = TRUE

u_getCountriesFromWikipedia = FALSE

# Files, directories and paths
u_path_rootData = "data"
u_fpath = list()
u_fpath$BP      = "BP/BP2016 - statistical-review-of-world-energy-2016-workbook.xlsx" 
u_fpath$BGR     = "BGR/BGR2016 - Energiestudie_2016_Tabellen.xlsx" 
u_fpath$IEAWEB  = "IEA/IEA2016 - World Energy Balances.csv"
u_fpath$IEACES  = "IEA/IEA2016 - Coal Energy Statistics.csv"
u_fpath$WDI     = "WDI/WDI_Data.csv"
u_fpath$COW     = "COW/"
u_fpath$CO2Typ  = "CO2 typology/typ_data_10_03_17.csv"
u_fpath$CO2TypM = "CO2 typology/typ_metadata_10_03_17.csv"

u_pathWikipediaCountryData <- "data/countries_wikipedia.RData"

u_outRData      = "data/coal_data.RData"

# Unit conversion
u_convert = list()
u_convert$Mtoe_to_GJ = 41868
u_convert$Mtoe_to_EJ = 41868*1e-9

# Accent conversion
# Adapted from http://stackoverflow.com/questions/20495598/replace-accented-characters-in-r-with-non-accented-counterpart-utf-8-encoding
# u_accentreplace = list(   'Š'='S',  'š'='s', 'Ž'='Z',  'ž'='z', 'À'='A', 'Á'='A',  'Â'='A', 'Ã'='A', 'Ä'='AE', 'Å'='A', 'Æ'='A',  'Ç'='C',  'È'='E', 'É'='E',
#                           'Ê'='E',  'Ë'='E', 'Ì'='I',  'Í'='I', 'Î'='I', 'Ï'='I',  'Ñ'='N', 'Ò'='O', 'Ó'='O',  'Ô'='O', 'Õ'='O',  'Ö'='OE', 'Ø'='O', 'Ù'='U',
#                           'Ú'='U',  'Û'='U', 'Ü'='UE', 'Ý'='Y', 'Þ'='B', 'ß'='Ss', 'à'='a', 'á'='a', 'â'='a',  'ã'='a', 'ä'='ae', 'å'='a',  'æ'='a', 'ç'='c',
#                           'è'='e',  'é'='e', 'ê'='e',  'ë'='e', 'ì'='i', 'í'='i',  'î'='i', 'ï'='i', 'ð'='o',  'ñ'='n', 'ò'='o',  'ó'='o',  'ô'='o', 'õ'='o',
#                           'ö'='oe', 'ø'='o', 'ù'='u',  'ú'='u', 'û'='u', 'ü'='ue', 'ý'='y', 'ý'='y', 'þ'='b',  'ÿ'='y' )


#---------------------------------------------
# INITIALISATION
#---------------------------------------------
library(dplyr)
library(tidyr)
library(openxlsx)
library(htmltab)
library(ggplot2)

u_fpath <- lapply(u_fpath, function(x) file.path(u_path_rootData, x))

v_data <- data.frame()

get_missing_countries <- function(i_data, i_countryList) {
  
  tmp <- unlist(sapply(unique(i_data$country), function(x) ifelse(length(which(i_countryList$country == x)) != 0, 1, 0)))
  
  missing_countries <- names(tmp)[which(tmp == 0)]
  
  print(missing_countries)
  
  return(missing_countries)
  
}

rename_countries <- function(i_data, i_nameMapping, i_countryList) {
  
  for (k in 1:length(i_nameMapping)) {
    i_data <- i_data %>% 
      mutate(country = ifelse(country == names(i_nameMapping)[k], paste(i_nameMapping[k]), country))
  }
  missing_countries <- get_missing_countries(i_data, i_countryList)
  
  i_data <- i_data %>% 
    filter(!country %in% missing_countries)
  
  return(i_data)
}

get_BPdata <- function(i_xlsxFile, i_sheet, i_startRow, i_variable, i_unit, i_longname, i_source, i_countryList, i_unitFactor=1) {
  
  if (DEBUG) cat(paste0(">> get_BPdata: ",i_variable," (",i_longname," - ",i_unit,")\n"))
  
  country_nameMapping <- c(
    "US"                  = "United States of America",
    "Venezuela"           = "Venezuela (Bolivarian Republic of)",
    "Trinidad & Tobago"   = "Trinidad and Tobago",
    "Czech Republic"      = "Czechia",
    "United Kingdom"      = "United Kingdom of Great Britain and Northern Ireland",
    "South Korea"         = "Korea (Republic of)",
    "Iran"                = "Iran (Islamic Republic of)",
    "Taiwan"              = "Taiwan, Province of China",
    "China Hong Kong SAR" = "Hong Kong",
    "Vietnam"             = "Viet Nam")
  
  tmp <- read.xlsx(xlsxFile = i_xlsxFile, sheet = i_sheet, startRow = i_startRow)
  
  if (i_sheet %in% c("Coal Consumption -  Mtoe", " Coal Production - Mtoe")) {
    tmp <- tmp[1:(which(grepl("Total.*World", tmp$Million.tonnes.oil.equivalent))-1), 1:(which(duplicated(names(tmp)))-1)]
    tmp <- tmp[-which(grepl("Total|Other", tmp$Million.tonnes.oil.equivalent)),]
    tmp <- tmp %>%
      rename(country = Million.tonnes.oil.equivalent) %>% 
      gather(year, value, -country) %>% 
      mutate(value    = as.numeric(value)*i_unitFactor) %>% 
      mutate(unit     = i_unit) %>% 
      mutate(variable = i_variable) %>% 
      mutate(longname = i_longname) %>% 
      mutate(source   = "BP 2016") %>% 
      select(country,year,variable,longname,source,unit,value) %>% 
      rename_countries(country_nameMapping, i_countryList)
  }
  if (i_sheet %in% c("Coal - Reserves")) {
    tmp <- tmp[1:(which(grepl("Total.*World", tmp$Million.tonnes))-1), which(names(tmp) %in% c("Million.tonnes", "Total"))]
    tmp <- tmp[-which(grepl("Total|Other", tmp$Million.tonnes)),]
    tmp <- tmp %>%
      rename(country = Million.tonnes) %>% 
      mutate(year=2015) %>% 
      rename(value=Total) %>% 
      mutate(value    = as.numeric(value)*i_unitFactor) %>% 
      mutate(unit     = i_unit) %>% 
      mutate(variable = i_variable) %>% 
      mutate(longname = i_longname) %>% 
      mutate(source   = "BP 2016") %>% 
      select(country,year,variable,longname,source,unit,value) %>% 
      rename_countries(country_nameMapping, i_countryList)
  }
  
  
  tmp <- i_countryList %>% 
    right_join(tmp,
               by=c("country"))
  
  return(tmp)
}

get_BGRdata <- function(i_xlsxFile, i_sheet, i_startRow, i_variable, i_unit, i_longname, i_source, i_countryList, i_unitFactor=1) {
  
  if (DEBUG) cat(paste0(">> get_BGRdata: ",i_variable," (",i_longname," - ",i_unit,")\n"))
  
  country_nameMapping <- c(
    "USA"                     = "United States of America",
    "Venezuela"               = "Venezuela (Bolivarian Republic of)",
    "Russische Föderation"    = "Russian Federation",
    "Australien"              = "Australia",
    "Südafrika"               = "South Africa",
    "Vereinigtes Königreich"  = "United Kingdom of Great Britain and Northern Ireland",
    "Kanada"                  = "Canada",
    "Kolumbien"               = "Colombia",
    "Indien"                  = "India",
    "Polen"                   = "Poland",
    "Kasachstan"              = "Kazakhstan",
    "Indonesien"              = "Indonesia",
    "Iran"                    = "Iran (Islamic Republic of)",
    "Deutschland"             = "Germany",
    "Mongolei"                = "Mongolia",
    "Kirgisistan"             = "Kyrgyzstan",
    "Simbabwe"                = "Zimbabwe",
    "Mosambik"                = "Mozambique",
    "Botsuana"                = "Botswana",
    "Tschechische Republik"   = "Czechia",
    "Brasilien"               = "Brazil",
    "Usbekistan"              = "Uzbekistan",
    "Mexiko"                  = "Mexico", 
    "Vietnam"                 = "Viet Nam",
    "Serbien"                 = "Serbia",
    "Rumänien"                = "Romania",
    "Argentinien"             = "Argentina",
    "Neuseeland"              = "New Zealand",
    "Türkei"                  = "Turkey",
    "Griechenland"            = "Greece",
    "Ungarn"                  = "Hungary",
    "Bosnien & Herzegowina"   = "Bosnia and Herzegovina",
    "Korea, Rep."             = "Korea (Republic of)",
    "Korea, DVR"              = "Korea (Democratic People's Republic of)",
    "Taiwan"                  = "Taiwan, Province of China",
    "Philippinen"             = "Philippines",
    "Niederlande"             = "Netherlands",
    "Italien"                 = "Italy",
    "Spanien"                 = "Spain",
    "Frankreich"              = "France",
    "Hongkong"                = "Hong Kong",
    "Bulgarien"               = "Bulgaria")
  
  tmp <- read.xlsx(xlsxFile = i_xlsxFile, sheet = i_sheet, startRow = i_startRow)
  
  if (i_sheet %in% c("31, Hartkohleberbrauch 2015",    "38, Weichbraunkohleverbrauc...", 
                     "28, Hartkohleressourcen 2015",   "29, Hartkohlereserven 2015", 
                     "35, Weichbraunkohleressourc...", "36, Weichbraunkohlereserven...",
                     "32, Hartkohleexport 2015",       "33, HartkohleImport 2015")) {
    tmp <- tmp[1:(which(grepl("sonstige L", tmp$`Land/Region`))-1), c(2,3)]
    tmp <- tmp %>%
      rename(country = `Land/Region`) %>% 
      # mutate(country = ifelse(length(which(sapply(names(u_accentreplace), function(x) grepl(x, "Türkei")))) != 0,
      #                         paste(sapply(names(which(sapply(names(u_accentreplace), function(x) grepl(x, country)))), 
      #                                      function(y) gsub(y, paste(u_accentreplace[[which(names(u_accentreplace) == y)]]), country))),
      #                         country)) %>% 
      mutate(year=2015) %>% 
      rename(value=Mt) %>% 
      mutate(value    = as.numeric(value)*i_unitFactor) %>% 
      mutate(unit     = i_unit) %>% 
      mutate(variable = i_variable) %>% 
      mutate(longname = i_longname) %>% 
      mutate(source   = i_source) %>% 
      select(country,year,variable,longname,source,unit,value) %>% 
      rename_countries(country_nameMapping, i_countryList)
  }
  
  if (i_sheet %in% c("30", "37", "30, Hartkohleförderung 2015", "37, Weichbraunkohleförderun...")) {
    tmp <- tmp[1:(which(grepl("sonstige L", tmp$`Land/Region`))-1), c(2,which(grepl("Mt", names(tmp))))]
    
    names(tmp)[which(grepl("Mt", names(tmp)))] <- substr(names(tmp)[which(grepl("Mt", names(tmp)))], 1,4)
    
    tmp <- tmp %>%
      rename(country = `Land/Region`) %>% 
      gather(year, value, -country) %>% 
      mutate(value    = as.numeric(value)*i_unitFactor) %>% 
      mutate(unit     = i_unit) %>% 
      mutate(variable = i_variable) %>% 
      mutate(longname = i_longname) %>% 
      mutate(source   = i_source) %>% 
      select(country,year,variable,longname,source,unit,value) %>% 
      rename_countries(country_nameMapping, i_countryList)
    
  }
  
  tmp <- i_countryList %>% 
    right_join(tmp,
               by=c("country"))
  
  return(tmp)
}

get_IEAWEBdata <- function(i_csvFile, i_product, i_flow, i_variable, i_unit, i_longname, i_source, i_countryList, i_unitFactor=1) {
  
  if (DEBUG) cat(paste0(">> get_IEAWEBdata: ",i_variable," (",i_longname," - ",i_unit,")\n"))
  
  country_nameMapping <- c(
    "China (P.R. of China and Hong Kong, China)" = "China",
    "Côte d'Ivoire"                              = "Cote d'Ivoire",
    "Bolivia"                                    = "Bolivia (Plurinational State of)",
    #"People's Republic of China"                 = "China",
    "Curaçao"                                    = "Curacao",
    "Czech Republic"                             = "Czechia",
    "Dem. People's Rep. of Korea"                = "Korea (Democratic People's Republic of)",
    "Dem. Republic of the Congo"                 = "Congo (Democratic Republic of the)",
    "Former Yugoslav Republic of Macedonia"      = "Macedonia (the former Yugoslav Republic of)",
    "Hong Kong, China"                           = "Hong Kong",
    "Islamic Republic of Iran"                   = "Iran (Islamic Republic of)",
    "Korea"                                      = "Korea (Republic of)",
    "Moldova"                                    = "Moldova (Republic of)",
    "Slovak Republic"                            = "Slovakia",
    "Chinese Taipei"                             = "Taiwan, Province of China",
    "Tanzania"                                   = "Tanzania, United Republic of",
    "United Kingdom"                             = "United Kingdom of Great Britain and Northern Ireland",
    "Venezuela"                                  = "Venezuela (Bolivarian Republic of)",
    "United States"                              = "United States of America")
  
  tmp <- read.csv(i_csvFile, stringsAsFactors = FALSE) %>% 
    filter(COUNTRY != "World") %>% 
    filter(PRODUCT == i_product) %>% 
    filter(FLOW    == i_flow)
  
  names(tmp)[which(grepl("X", names(tmp)))] <- substr(names(tmp)[which(grepl("X", names(tmp)))], 2,5)
  names(tmp) <- tolower(names(tmp))
  
  tmp <- tmp %>% 
    gather(year, value, -country, -product, -flow) %>% 
    mutate(value    = as.numeric(value)*i_unitFactor) %>% 
    mutate(unit     = i_unit) %>% 
    mutate(variable = i_variable) %>% 
    mutate(longname = i_longname) %>% 
    mutate(source   = i_source) %>% 
    select(country,year,variable,longname,source,unit,value) %>% 
    rename_countries(country_nameMapping, i_countryList)
  
  tmp <- i_countryList %>% 
    right_join(tmp,
               by=c("country"))
  
  return(tmp)
  
}

get_WDIdata <- function(i_csvFile, i_indicator, i_variable, i_unit, i_longname, i_source, i_countryList, i_unitFactor=1) {
  
  if (DEBUG) cat(paste0(">> get_WDIdata: ",i_variable," (",i_longname," - ",i_unit,")\n"))
  
  country_nameMapping <- c(
    "China (P.R. of China and Hong Kong, China)" = "China",
    "Côte d'Ivoire"                              = "Cote d'Ivoire",
    "Bolivia"                                    = "Bolivia (Plurinational State of)",
    #"People's Republic of China"                 = "China",
    "Curaçao"                                    = "Curacao",
    "Czech Republic"                             = "Czechia",
    "Dem. People's Rep. of Korea"                = "Korea (Democratic People's Republic of)",
    "Dem. Republic of the Congo"                 = "Congo (Democratic Republic of the)",
    "Former Yugoslav Republic of Macedonia"      = "Macedonia (the former Yugoslav Republic of)",
    "Hong Kong, China"                           = "Hong Kong",
    "Islamic Republic of Iran"                   = "Iran (Islamic Republic of)",
    "Iran, Islamic Rep."                         = "Iran (Islamic Republic of)",
    "Korea"                                      = "Korea (Republic of)",
    "Vietnam"                                    = "Viet Nam",
    "Kyrgyz Republic"                            = "Kyrgyzstan",
    "Moldova"                                    = "Moldova (Republic of)",
    "Slovak Republic"                            = "Slovakia",
    "Chinese Taipei"                             = "Taiwan, Province of China",
    "Tanzania"                                   = "Tanzania, United Republic of",
    "United Kingdom"                             = "United Kingdom of Great Britain and Northern Ireland",
    "Venezuela"                                  = "Venezuela (Bolivarian Republic of)",
    "Hong Kong SAR, China"                       = "Hong Kong",
    "Korea, Rep."                                = "Korea (Republic of)",
    "Korea, Dem. People’s Rep."                  = "Korea (Democratic People's Republic of)",
    "United States"                              = "United States of America",
    "Congo, Rep."                                = "Congo",
    "Congo, Dem. Rep."                           = "Congo (Democratic Republic of the)",
    "Egypt, Arab Rep."                           = "Egypt",
    "Gambia, The"                                = "Gambia",
    "Bahamas, The"                               = "Bahamas",
    "British Virgin Islands"                     = "", 
    #"Channel Islands"                            = "",
    "Korea, Dem. People’s Rep."                  = "Korea (Democratic People's Republic of)",
    "Lao PDR"                                    = "Lao People's Democratic Republic",
    "Macao SAR, China"                           = "Macao",
    "Macedonia, FYR"                             = "Macedonia (the former Yugoslav Republic of)",
    "Micronesia, Fed. Sts."                      = "Micronesia (Federated States of)",
    "St. Kitts and Nevis"                        = "Saint Kitts and Nevis",
    "St. Lucia"                                  = "Saint Lucia",
    "St. Martin (French part)"                   = "Saint Martin (French part)", 
    "St. Vincent and the Grenadines"             = "Saint Vincent and the Grenadines", 
    "Venezuela, RB"                              = "Venezuela (Bolivarian Republic of)",
    #"West Bank and Gaza"                         = "", 
    "Yemen, Rep."                                = "Yemen")
  
  RDatafile <- file.path(dirname(i_csvFile), paste0(strsplit(basename(i_csvFile), ".", fixed=TRUE)[[1]][1], ".RData"))
  if (!file.exists(RDatafile)) {
    tmp <- read.csv(i_csvFile, stringsAsFactors = FALSE)
    save(tmp, file = RDatafile)
  } else {
    load(RDatafile)
  }
  
  tmp <- tmp %>% 
    filter(Indicator.Name == i_indicator)
  
  rm_countries <- unique(tmp$Country.Name)[1:47]
  
  tmp <- tmp %>% 
    filter(!Country.Name %in% rm_countries)
  
  names(tmp)[which(grepl("X", names(tmp)))] <- substr(names(tmp)[which(grepl("X", names(tmp)))], 2,5)
  names(tmp) <- tolower(names(tmp))
  
  tmp <- tmp %>% 
    rename(country = country.name) %>% 
    rename(iso     = country.code) %>% 
    rename(name    = indicator.name) %>% 
    rename(code    = indicator.code) %>% 
    gather(year, value, -country, -iso, -name, -code) %>% 
    mutate(value    = as.numeric(value)*i_unitFactor) %>% 
    mutate(unit     = i_unit) %>% 
    mutate(variable = i_variable) %>% 
    mutate(longname = i_longname) %>% 
    mutate(source   = i_source) %>% 
    select(country,year,variable,longname,source,unit,value) %>% 
    rename_countries(country_nameMapping, i_countryList)
  
  tmp <- i_countryList %>% 
    right_join(tmp,
               by=c("country"))
  
  return(tmp)
  
}  

#---------------------------------------------
# GET AND PROCESS DATA
#---------------------------------------------
#-- Get country list -------------------------
if (u_getCountriesFromWikipedia) {
  # Source: Wikipedia
  url     <- "https://en.wikipedia.org/wiki/ISO_3166-1"
  countries <- htmltab(doc = url, which = "//th[text() = 'English short name (upper/lower case)']/ancestor::table") %>% 
    select(-`Link to ISO 3166-2 subdivision codes`, -`Alpha-2 code`, -`Numeric code`, - `Independent?`) %>% 
    rename(country = `English short name (upper/lower case)`) %>% 
    rename(iso  = `Alpha-3 code`)
  # Correct/remove accents
  countries$country <- gsub("Ã´", "o", countries$country)
  countries$country <- gsub("Ã§", "c", countries$country)
  countries$country <- gsub("Ã©", "e", countries$country)
  countries$country <- gsub("Ã…", "A", countries$country)
  countries$country <- gsub("C4", "o", countries$country)
  countries$country <- gsub("C'", "c", countries$country)
  countries$country <- gsub("C)", "e", countries$country)
  countries$country <- gsub("C ", "A", countries$country, fixed = TRUE)
  countries <- countries %>% 
    mutate(country = ifelse(iso == "ALA", "Aland Island", country)) 
  
  # Add missing countries
  countries <- rbind(countries,
                     data.frame(country = "Kosovo", iso = "UNK")) %>% 
    arrange(country)
  
  save(countries, file = u_pathWikipediaCountryData)
  
} else {
  load(u_pathWikipediaCountryData)
}
#-- Coal consumption -------------------------
# Source: BP 2016
#  - Reallocate category "Other *" using proxy
#  - Process USSR
v_data <- rbind(v_data,
                get_BPdata(u_fpath$BP, "Coal Consumption -  Mtoe", 3,
                           "E_CC", "GJ/yr", "Coal consumption", "BP 2016", 
                           countries, i_unitFactor=u_convert$Mtoe_to_GJ))

# Source: BGR 2016
# TODO:
#  - Ask Andruleit to provide full tables for oil, coal and gas (reserves, resources, production, demand, import, export)
v_data <- rbind(v_data,
                get_BGRdata(u_fpath$BGR, "31, Hartkohleberbrauch 2015", 1,
                            "E_CC", "Mt(coal)", "Coal consumption - Hard coal", "BGR 2016", 
                            countries))

v_data <- rbind(v_data,
                get_BGRdata(u_fpath$BGR, "38, Weichbraunkohleverbrauc...", 1,
                            "E_CC", "Mt(coal)", "Coal consumption - Lignite coal", "BGR 2016", 
                            countries))

# Source: IEA WEB 2016
# TODO:
v_data <- rbind(v_data,
                get_IEAWEBdata(u_fpath$IEAWEB, "Coal and coal products", "Total final consumption", 
                               "E_CC", "GJ/yr", "Coal consumption", "IEA2016 - WEB", countries))


#-- Coal production -------------------------
# Source: BP 2016
#  - Reallocate category "Other *" using proxy
#  - Process USSR
v_data <- rbind(v_data,
                get_BPdata(u_fpath$BP, " Coal Production - Mtoe", 3,
                           "E_CP", "GJ/yr", "Coal Production", "BP 2016", 
                           countries, i_unitFactor=u_convert$Mtoe_to_GJ))

# Source: BGR 2016
# TODO:
#  - Ask Andruleit to provide full tables for oil, coal and gas (reserves, resources, production, demand, import, export)
#  - Process Kosovo
#  - Problem with character encoding
v_data <- rbind(v_data,
                get_BGRdata(u_fpath$BGR, 30, 1,
                            "E_CP", "Mt(coal)", "Coal Production - Hard coal", "BGR 2016", 
                            countries))

v_data <- rbind(v_data,
                get_BGRdata(u_fpath$BGR, 37, 1,
                            "E_CP", "Mt(coal)", "Coal Production - Lignite coal", "BGR 2016", 
                            countries))

# Source: IEA WEB 2016
# TODO:
v_data <- rbind(v_data,
                get_IEAWEBdata(u_fpath$IEAWEB, "Coal and coal products", "Production", 
                               "E_CP", "Mt/yr", "Coal production", "IEA2016 - WEB", countries))

#-- Coal stocks ------------------------------
# Source: IEA WEB 2016
# TODO:
v_data <- rbind(v_data,
                get_IEAWEBdata(u_fpath$IEAWEB, "Coal and coal products", "Stock changes", 
                               "E_CSt", "Mt/yr", "Coal stock changes", "IEA2016 - WEB", countries))

#-- Coal imports/exports ---------------------
# Source: BGR 2016
# TODO:
#  - Ask Andruleit to provide full tables for oil, coal and gas (reserves, resources, production, demand, import, export)
#  - Process Kosovo
#  - Problem with character encoding
v_data <- rbind(v_data,
                get_BGRdata(u_fpath$BGR, "33, HartkohleImport 2015", 1,
                            "E_CIm", "Mt(coal)", "Coal imports - Hard coal", "BGR 2016", 
                            countries))

v_data <- rbind(v_data,
                get_BGRdata(u_fpath$BGR, "32, Hartkohleexport 2015", 1,
                            "E_CEx", "Mt(coal)", "Coal Exports - Hard coal", "BGR 2016", 
                            countries))

# Source: IEA WEB 2016
# TODO:
v_data <- rbind(v_data,
                get_IEAWEBdata(u_fpath$IEAWEB, "Coal and coal products", "Imports", 
                               "E_CIm", "Mt/yr", "Coal imports", "IEA2016 - WEB", countries))

v_data <- rbind(v_data,
                get_IEAWEBdata(u_fpath$IEAWEB, "Coal and coal products", "Exports", 
                               "E_CEx", "Mt/yr", "Coal exports", "IEA2016 - WEB", countries))

#-- Electricity production from coal ---------
# Source: IEA WEB 2016
# TODO:
v_data <- rbind(v_data,
                get_IEAWEBdata(u_fpath$IEAWEB, "Coal and coal products", "Electricity output (GWh)", 
                               "Elec_C", "GWh", "Coal - Electricity Output", "IEA2016 - WEB", countries))

# Source: WB 2016
v_data <- rbind(v_data,
                get_WDIdata(u_fpath$WDI, "Electricity production from coal sources (% of total)", 
                            "Elec_C", "% of total", "Electricity production from coal sources", "WDI 2016", 
                            countries))


#-- Coal use in sectors (Industry, Commercial, Residential ...) -----------
# Source: IEA WEB 2016
# TODO:
v_data <- rbind(v_data,
                get_IEAWEBdata(u_fpath$IEAWEB, "Coal and coal products", "Industry", 
                               "E_CSeInd", "Mt/yr", "Coal - Industry", "IEA2016 - WEB", countries))
v_data <- rbind(v_data,
                get_IEAWEBdata(u_fpath$IEAWEB, "Coal and coal products", "Agriculture/forestry", 
                               "E_CSeInAgFo", "Mt/yr", "Coal - Agriculture & forestry", "IEA2016 - WEB", countries))
v_data <- rbind(v_data,
                get_IEAWEBdata(u_fpath$IEAWEB, "Coal and coal products", "Iron and steel", 
                               "E_CSeInIS", "Mt/yr", "Coal - Iron and steel", "IEA2016 - WEB", countries))
v_data <- rbind(v_data,
                get_IEAWEBdata(u_fpath$IEAWEB, "Coal and coal products", "Chemical and petrochemical", 
                               "E_CSeInCP", "Mt/yr", "Coal - Chemical and petrochemical", "IEA2016 - WEB", countries))
v_data <- rbind(v_data,
                get_IEAWEBdata(u_fpath$IEAWEB, "Coal and coal products", "Textile and leather", 
                               "E_CSeInTL", "Mt/yr", "Coal - Textile and leather", "IEA2016 - WEB", countries))
v_data <- rbind(v_data,
                get_IEAWEBdata(u_fpath$IEAWEB, "Coal and coal products", "Paper, pulp and printing ", 
                               "E_CSeInPPP", "Mt/yr", "Coal - Paper, pulp and printing", "IEA2016 - WEB", countries))
v_data <- rbind(v_data,
                get_IEAWEBdata(u_fpath$IEAWEB, "Coal and coal products", "Mining and quarrying", 
                               "E_CSeInMQ", "Mt/yr", "Coal - Mining and quarrying", "IEA2016 - WEB", countries))
v_data <- rbind(v_data,
                get_IEAWEBdata(u_fpath$IEAWEB, "Coal and coal products", "Non-ferrous metals", 
                               "E_CSeInNFM", "Mt/yr", "Coal - Non-ferrous metals", "IEA2016 - WEB", countries))
v_data <- rbind(v_data,
                get_IEAWEBdata(u_fpath$IEAWEB, "Coal and coal products", "Non-metallic minerals", 
                               "E_CSeInNMM", "Mt/yr", "Coal - Non-metallic minerals", "IEA2016 - WEB", countries))
v_data <- rbind(v_data,
                get_IEAWEBdata(u_fpath$IEAWEB, "Coal and coal products", "Food and tobacco", 
                               "E_CSeInFT", "Mt/yr", "Coal - Food and tobacco", "IEA2016 - WEB", countries))
v_data <- rbind(v_data,
                get_IEAWEBdata(u_fpath$IEAWEB, "Coal and coal products", "Wood and wood products", 
                               "E_CSeInWWP", "Mt/yr", "Coal - Wood and wood products", "IEA2016 - WEB", countries))
v_data <- rbind(v_data,
                get_IEAWEBdata(u_fpath$IEAWEB, "Coal and coal products", "Construction", 
                               "E_CSeInCo", "Mt/yr", "Coal - Construction", "IEA2016 - WEB", countries))
v_data <- rbind(v_data,
                get_IEAWEBdata(u_fpath$IEAWEB, "Coal and coal products", "Non-specified (industry)", 
                               "E_CSeInNA", "Mt/yr", "Coal - Non-specified (industry)", "IEA2016 - WEB", countries))
v_data <- rbind(v_data,
                get_IEAWEBdata(u_fpath$IEAWEB, "Coal and coal products", "Machinery", 
                               "E_CSeInMa", "Mt/yr", "Coal - Machinery", "IEA2016 - WEB", countries))
v_data <- rbind(v_data,
                get_IEAWEBdata(u_fpath$IEAWEB, "Coal and coal products", "Commercial and public services", 
                               "E_CSeCoPuS", "Mt/yr", "Coal - Commercial and public services", "IEA2016 - WEB", countries))
v_data <- rbind(v_data,
                get_IEAWEBdata(u_fpath$IEAWEB, "Coal and coal products", "Residential", 
                               "E_CRes", "Mt/yr", "Coal - Residential", "IEA2016 - WEB", countries))
v_data <- rbind(v_data,
                get_IEAWEBdata(u_fpath$IEAWEB, "Coal and coal products", "Transport", 
                               "E_CSeTr", "Mt/yr", "Coal - Transport (Road, Rail, Dom. Navigation...)", "IEA2016 - WEB", countries))
v_data <- rbind(v_data,
                get_IEAWEBdata(u_fpath$IEAWEB, "Coal and coal products", "Road", 
                               "E_CSeRo", "Mt/yr", "Coal - Road", "IEA2016 - WEB", countries))
v_data <- rbind(v_data,
                get_IEAWEBdata(u_fpath$IEAWEB, "Coal and coal products", "Rail", 
                               "E_CSeRa", "Mt/yr", "Coal - Rail", "IEA2016 - WEB", countries))
v_data <- rbind(v_data,
                get_IEAWEBdata(u_fpath$IEAWEB, "Coal and coal products", "Domestic navigation", 
                               "E_CSeNav", "Mt/yr", "Coal - Domestic navigation", "IEA2016 - WEB", countries))
v_data <- rbind(v_data,
                get_IEAWEBdata(u_fpath$IEAWEB, "Coal and coal products", "Energy industry own use", 
                               "E_CSeEx", "Mt/yr", "Coal - Energy industry own use", "IEA2016 - WEB", countries))


#-- Coal, oil, gas endowments ----------------
# Source: BP 2016
#  - Reallocate category "Other *" using proxy
#  - Process USSR
#  - Split hard coal and lignite
v_data <- rbind(v_data,
                get_BPdata(u_fpath$BP, "Coal - Reserves", 4,
                           "E_CE", "Mt(coal)", "Coal endowment - Reserves (proved at the end of 2015)", "BP 2016", 
                           countries))

# Source: BGR 2016
# TODO:
#  - Ask Andruleit to provide full tables for oil, coal and gas (reserves, resources, production, demand, import, export)
#  - Process Kosovo
v_data <- rbind(v_data,
                get_BGRdata(u_fpath$BGR, "28, Hartkohleressourcen 2015", 1,
                            "E_CE", "Mt(coal)", "Coal endowment - Hard coal Resources (proved at the end of 2015)", "BGR 2016", 
                            countries))
v_data <- rbind(v_data,
                get_BGRdata(u_fpath$BGR, "29, Hartkohlereserven 2015", 1,
                            "E_CE", "Mt(coal)", "Coal endowment - Hard coal Reserves (proved at the end of 2015)", "BGR 2016", 
                            countries))

v_data <- rbind(v_data,
                get_BGRdata(u_fpath$BGR, "35, Weichbraunkohleressourc...", 1,
                            "E_CE", "Mt(coal)", "Coal endowment - Lignite Resources (proved at the end of 2015)", "BGR 2016", 
                            countries))
v_data <- rbind(v_data,
                get_BGRdata(u_fpath$BGR, "36, Weichbraunkohlereserven...", 1,
                            "E_CE", "Mt(coal)", "Coal endowment - Lignite Reserves (proved at the end of 2015)", "BGR 2016", 
                            countries))


#-- Coal rents -------------------------------
# Source: WB 2016
v_data <- rbind(v_data,
                get_WDIdata(u_fpath$WDI, "Coal rents (% of GDP)", 
                            "E_CRe", "%(GDP)", "Coal rents", "WDI 2016", 
                            countries))


#-- Natural gas consumption ------------------
# Source: IEA WEB 2016
# TODO:
v_data <- rbind(v_data,
                get_IEAWEBdata(u_fpath$IEAWEB, "Natural gas", "Total final consumption", 
                               "E_GC", "Mt/yr", "Natural gas consumption", "IEA2016 - WEB", countries))

#-- Oil products consumption ------------------
# Source: IEA WEB 2016
# TODO:
v_data <- rbind(v_data,
                get_IEAWEBdata(u_fpath$IEAWEB, "Oil products", "Total final consumption", 
                               "E_OC", "Mt/yr", "Oil products consumption", "IEA2016 - WEB", countries))

#-- Nuclear consumption -----------------------
# Source: IEA WEB 2016
# TODO:
v_data <- rbind(v_data,
                get_IEAWEBdata(u_fpath$IEAWEB, "Nuclear", "Total final consumption", 
                               "E_NC", "Mt/yr", "Nuclear consumption", "IEA2016 - WEB", countries))

#-- Hydro consumption -------------------------
# Source: IEA WEB 2016
# TODO:
v_data <- rbind(v_data,
                get_IEAWEBdata(u_fpath$IEAWEB, "Hydro", "Total final consumption", 
                               "E_HC", "Mt/yr", "Hydro consumption", "IEA2016 - WEB", countries))

#-- Geothermal consumption -------------------------
# Source: IEA WEB 2016
# TODO:
v_data <- rbind(v_data,
                get_IEAWEBdata(u_fpath$IEAWEB, "Geothermal", "Total final consumption", 
                               "E_GeoC", "Mt/yr", "Geothermal consumption", "IEA2016 - WEB", countries))

#-- Solar/wind/other consumption -------------------------
# Source: IEA WEB 2016
# TODO:
v_data <- rbind(v_data,
                get_IEAWEBdata(u_fpath$IEAWEB, "Solar/wind/other", "Total final consumption", 
                               "E_RC", "Mt/yr", "Solar/wind/other consumption", "IEA2016 - WEB", countries))

#-- Biofuels and waste consumption -------------------------
# Source: IEA WEB 2016
# TODO:
v_data <- rbind(v_data,
                get_IEAWEBdata(u_fpath$IEAWEB, "Biofuels and waste", "Total final consumption", 
                               "E_BC", "Mt/yr", "Biofuels and waste consumption", "IEA2016 - WEB", countries))

#-- Electricity consumption -------------------------
# Source: IEA WEB 2016
# TODO:
v_data <- rbind(v_data,
                get_IEAWEBdata(u_fpath$IEAWEB, "Electricity", "Total final consumption", 
                               "Elec", "Mt/yr", "Electricity consumption", "IEA2016 - WEB", countries))


#-- GDP --------------------------------------
# Source: WB 2016
v_data <- rbind(v_data,
                get_WDIdata(u_fpath$WDI, "GDP, PPP (constant 2011 international $)", 
                            "GDP", "PPP (constant 2011 international $)", "GDP", "WDI 2016", 
                            countries))

#-- GDP per capita ---------------------------
# Source: WB 2016
v_data <- rbind(v_data,
                get_WDIdata(u_fpath$WDI, "GDP per capita, PPP (constant 2011 international $)", 
                            "GDPpc", "PPP (constant 2011 international $)", "GDP per capita", "WDI 2016", 
                            countries))

#-- Air pollution ---------------------------
# Source: WB 2016
v_data <- rbind(v_data,
                get_WDIdata(u_fpath$WDI, "PM2.5 air pollution, mean annual exposure (micrograms per cubic meter)", 
                            "AP_pm25mae", "micrograms per cubic meter", "PM2.5 air pollution (mean annual exposure)", "WDI 2016", 
                            countries))
v_data <- rbind(v_data,
                get_WDIdata(u_fpath$WDI, "PM2.5 air pollution, population exposed to levels exceeding WHO guideline value (% of total)", 
                            "AP_pm25pe", "% of total population", "PM2.5 air pollution (population exposed to levels exceeding WHO guideline value)", "WDI 2016", 
                            countries))

#-- Non-dependent population -----------------
v_data <- rbind(v_data,
                get_WDIdata(u_fpath$WDI, "Population ages 15-64 (% of total)", 
                            "P_Ndep", "% of total population", "Population ages 15-64", "WDI 2016", 
                            countries))

#-- Manufacturing ----------------------------
v_data <- rbind(v_data,
                get_WDIdata(u_fpath$WDI, "Manufacturing, value added (% of GDP)", 
                            "Manu", "% of GDP", "Manufacturing, value added", "WDI 2016", 
                            countries))

#-- Manufacture exports ----------------------
v_data <- rbind(v_data,
                get_WDIdata(u_fpath$WDI, "Manufactures exports (% of merchandise exports)", 
                            "Manu_Ex", "% of merchandise exports", "Manufactures exports", "WDI 2016", 
                            countries))

#---------------------------------------------
# COMBINE WITH CO2 typology data
#---------------------------------------------
v_CO2TypRDataPath <- strsplit(u_fpath$CO2Typ, ".", fixed=TRUE)[[1]]
v_CO2TypRDataPath <- paste0(paste0(v_CO2TypRDataPath[1:(length(v_CO2TypRDataPath)-1)], collapse="."), ".RData")
if (!file.exists(v_CO2TypRDataPath)) {
  p_data <- read.csv(u_fpath$CO2Typ)
  save(p_data, file = v_CO2TypRDataPath)
  
} else {
  load(v_CO2TypRDataPath)
}


p_data <- p_data %>% 
  gather(variable, value, -country, -year) %>% 
  mutate(longname = "") %>% 
  mutate(source   = "") %>% 
  mutate(unit     = "") %>% 
  mutate(longname = ifelse(variable == "pop_UN", "Population", longname)) %>% 
  mutate(source   = ifelse(variable == "pop_UN", "UN", source)) %>% 
  mutate(unit     = ifelse(variable == "pop_UN", "million Inhabitants", unit)) %>% 
  mutate(variable = ifelse(variable == "pop_UN", "P", variable)) %>% 
  mutate(longname = ifelse(variable == "gdp_ppp_PWT", "Expenditure-side real GDP at chained PPPs", longname)) %>% 
  mutate(source   = ifelse(variable == "gdp_ppp_PWT", "Penn World Tables", source)) %>% 
  mutate(unit     = ifelse(variable == "gdp_ppp_PWT", "million (2011) US$", unit)) %>% 
  mutate(variable = ifelse(variable == "gdp_ppp_PWT", "GDP", variable)) %>% 
  mutate(longname = ifelse(variable == "lex_WB", "Life expectancy at birth", longname)) %>% 
  mutate(source   = ifelse(variable == "lex_WB", "World Bank (WDI)", source)) %>% 
  mutate(unit     = ifelse(variable == "lex_WB", "Total (years)", unit)) %>% 
  mutate(variable = ifelse(variable == "lex_WB", "LEx", variable)) %>% 
  mutate(longname = ifelse(variable == "gini_WB", "GINI index (World Bank estimate)", longname)) %>% 
  mutate(source   = ifelse(variable == "gini_WB", "World Bank (WDI)", source)) %>% 
  mutate(unit     = ifelse(variable == "gini_WB", "Unitless", unit)) %>%
  mutate(variable = ifelse(variable == "gini_WB", "GINI", variable)) %>% 
  mutate(longname = ifelse(variable == "urban_WB", "Urban population", longname)) %>% 
  mutate(source   = ifelse(variable == "urban_WB", "World Bank (WDI)", source)) %>% 
  mutate(unit     = ifelse(variable == "urban_WB", "%", unit)) %>% 
  mutate(variable = ifelse(variable == "urban_WB", "URB", variable)) %>% 
  mutate(longname = ifelse(variable == "pop_density_WB", "Population density", longname)) %>% 
  mutate(source   = ifelse(variable == "pop_density_WB", "World Bank (WDI)", source)) %>% 
  mutate(unit     = ifelse(variable == "pop_density_WB", "people per squared km of land area", unit)) %>% 
  mutate(variable = ifelse(variable == "pop_density_WB", "PDen", variable)) %>% 
  mutate(longname = ifelse(variable == "share_gdp_agri_WB", "Agricultural share of GDP", longname)) %>% 
  mutate(source   = ifelse(variable == "share_gdp_agri_WB", "World Bank (WDI)", source)) %>% 
  mutate(unit     = ifelse(variable == "share_gdp_agri_WB", "% of GDP", unit)) %>% 
  mutate(variable = ifelse(variable == "share_gdp_agri_WB", "GDP_Ag", variable)) %>% 
  mutate(longname = ifelse(variable == "share_gdp_ind_WB", "Industrial share of GDP", longname)) %>% 
  mutate(source   = ifelse(variable == "share_gdp_ind_WB", "World Bank (WDI)", source)) %>% 
  mutate(unit     = ifelse(variable == "share_gdp_ind_WB", "% of GDP", unit)) %>% 
  mutate(variable = ifelse(variable == "share_gdp_ind_WB", "GDP_Ind", variable)) %>% 
  mutate(longname = ifelse(variable == "share_gdp_serv_WB", "Services share of GDP", longname)) %>% 
  mutate(source   = ifelse(variable == "share_gdp_serv_WB", "World Bank (WDI)", source)) %>% 
  mutate(unit     = ifelse(variable == "share_gdp_serv_WB", "% of GDP", unit)) %>% 
  mutate(variable = ifelse(variable == "share_gdp_serv_WB", "GDP_Ser", variable)) %>%
  mutate(longname = ifelse(variable == "share_gdp_trade_WB", "Trade share of GDP", longname)) %>% 
  mutate(source   = ifelse(variable == "share_gdp_trade_WB", "World Bank (WDI)", source)) %>% 
  mutate(unit     = ifelse(variable == "share_gdp_trade_WB", "% of GDP", unit)) %>% 
  mutate(variable = ifelse(variable == "share_gdp_trade_WB", "GDP_Tra", variable)) %>% 
  mutate(longname = ifelse(variable == "co2_terr_CDIAC", "CO2 emissions", longname)) %>% 
  mutate(source   = ifelse(variable == "co2_terr_CDIAC", "CDIAC", source)) %>% 
  mutate(unit     = ifelse(variable == "co2_terr_CDIAC", "Gg(CO2)/yr", unit)) %>% 
  mutate(variable = ifelse(variable == "co2_terr_CDIAC", "CO2", variable)) %>% 
  mutate(longname = ifelse(variable == "co2_terr_GCB", "CO2 emissions", longname)) %>% 
  mutate(source   = ifelse(variable == "co2_terr_GCB", "GCB", source)) %>% 
  mutate(unit     = ifelse(variable == "co2_terr_GCB", "Gg(CO2)/yr", unit)) %>% 
  mutate(variable = ifelse(variable == "co2_terr_GCB", "CO2", variable)) %>%
  mutate(longname = ifelse(variable == "co2_cons_GCB", "CO2 emissions from consumption", longname)) %>% 
  mutate(source   = ifelse(variable == "co2_cons_GCB", "GCB", source)) %>% 
  mutate(unit     = ifelse(variable == "co2_cons_GCB", "Gg(CO2)/yr", unit)) %>% 
  mutate(variable = ifelse(variable == "co2_cons_GCB", "CO2C", variable)) %>% 
  mutate(longname = ifelse(variable == "energy_tpes_IEA", "Total primary energy consumption", longname)) %>% 
  mutate(source   = ifelse(variable == "energy_tpes_IEA", "IEA", source)) %>% 
  mutate(unit     = ifelse(variable == "energy_tpes_IEA", "GJ/yr", unit)) %>% 
  mutate(variable = ifelse(variable == "energy_tpes_IEA", "EnPri", variable)) %>% 
  mutate(longname = ifelse(variable == "energy_tfec_IEA", "Total final energy consumption", longname)) %>% 
  mutate(source   = ifelse(variable == "energy_tfec_IEA", "IEA", source)) %>% 
  mutate(unit     = ifelse(variable == "energy_tfec_IEA", "GJ/yr", unit)) %>% 
  mutate(variable = ifelse(variable == "energy_tfec_IEA", "EnFin", variable)) %>% 
  mutate(longname = ifelse(variable == "gini_SWIID", "Gini coefficient", longname)) %>% 
  mutate(source   = ifelse(variable == "gini_SWIID", "SWIID", source)) %>% 
  mutate(unit     = ifelse(variable == "gini_SWIID", "", unit)) %>% 
  mutate(variable = ifelse(variable == "gini_SWIID", "GINI", variable)) %>% 
  mutate(longname = ifelse(variable == "inst_WGI", "Institution quality index", longname)) %>% 
  mutate(source   = ifelse(variable == "inst_WGI", "UN", source)) %>% 
  mutate(unit     = ifelse(variable == "inst_WGI", "Unitless", unit)) %>% 
  mutate(variable = ifelse(variable == "inst_WGI", "Inst", variable)) %>% 
  mutate(longname = ifelse(variable == "dummy_oil", "Oil endowment", longname)) %>% 
  mutate(source   = ifelse(variable == "dummy_oil", "?", source)) %>% 
  mutate(unit     = ifelse(variable == "dummy_oil", "Unitless", unit)) %>%
  mutate(variable = ifelse(variable == "dummy_oil", "E_OE", variable)) %>% 
  mutate(longname = ifelse(variable == "dummy_gas", "Gas endowment", longname)) %>% 
  mutate(source   = ifelse(variable == "dummy_gas", "?", source)) %>% 
  mutate(unit     = ifelse(variable == "dummy_gas", "Unitless", unit)) %>% 
  mutate(variable = ifelse(variable == "dummy_gas", "E_GE", variable)) %>%
  mutate(longname = ifelse(variable == "dummy_coal", "Coal endowment", longname)) %>% 
  mutate(source   = ifelse(variable == "dummy_coal", "?", source)) %>% 
  mutate(unit     = ifelse(variable == "dummy_coal", "Unitless", unit)) %>% 
  mutate(variable = ifelse(variable == "dummy_coal", "E_CE", variable)) %>%
  mutate(longname = ifelse(variable == "energy_coal_production_IEA", "Coal production", longname)) %>% 
  mutate(source   = ifelse(variable == "energy_coal_production_IEA", "IEA", source)) %>% 
  mutate(unit     = ifelse(variable == "energy_coal_production_IEA", "EJ/yr", unit)) %>% 
  mutate(variable = ifelse(variable == "energy_coal_production_IEA", "E_CP", variable)) %>% 
  mutate(longname = ifelse(variable == "energy_oil_production_IEA", "Oil production", longname)) %>% 
  mutate(source   = ifelse(variable == "energy_oil_production_IEA", "IEA", source)) %>% 
  mutate(unit     = ifelse(variable == "energy_oil_production_IEA", "EJ/yr", unit)) %>% 
  mutate(variable = ifelse(variable == "energy_oil_production_IEA", "E_OP", variable)) %>% 
  mutate(longname = ifelse(variable == "energy_gas_production_IEA", "Gas production", longname)) %>% 
  mutate(source   = ifelse(variable == "energy_gas_production_IEA", "IEA", source)) %>% 
  mutate(unit     = ifelse(variable == "energy_gas_production_IEA", "EJ/yr", unit)) %>% 
  mutate(variable = ifelse(variable == "energy_gas_production_IEA", "E_GP", variable)) %>% 
  mutate(longname = ifelse(variable == "energy_coal_imports_IEA", "Coal imports", longname)) %>% 
  mutate(source   = ifelse(variable == "energy_coal_imports_IEA", "IEA", source)) %>% 
  mutate(unit     = ifelse(variable == "energy_coal_imports_IEA", "EJ/yr", unit)) %>% 
  mutate(variable = ifelse(variable == "energy_coal_imports_IEA", "E_CIm", variable)) %>% 
  mutate(longname = ifelse(variable == "energy_oil_imports_IEA", "Oil imports", longname)) %>% 
  mutate(source   = ifelse(variable == "energy_oil_imports_IEA", "IEA", source)) %>% 
  mutate(unit     = ifelse(variable == "energy_oil_imports_IEA", "EJ/yr", unit)) %>% 
  mutate(variable = ifelse(variable == "energy_oil_imports_IEA", "E_OIm", variable)) %>% 
  mutate(longname = ifelse(variable == "energy_gas_imports_IEA", "Gas imports", longname)) %>% 
  mutate(source   = ifelse(variable == "energy_gas_imports_IEA", "IEA", source)) %>% 
  mutate(unit     = ifelse(variable == "energy_gas_imports_IEA", "EJ/yr", unit)) %>% 
  mutate(variable = ifelse(variable == "energy_gas_imports_IEA", "E_GIm", variable)) %>% 
  mutate(longname = ifelse(variable == "energy_coal_exports_IEA", "Coal exports", longname)) %>% 
  mutate(source   = ifelse(variable == "energy_coal_exports_IEA", "IEA", source)) %>% 
  mutate(unit     = ifelse(variable == "energy_coal_exports_IEA", "EJ/yr", unit)) %>%
  mutate(variable = ifelse(variable == "energy_coal_exports_IEA", "E_Cex", variable)) %>%
  mutate(longname = ifelse(variable == "energy_oil_exports_IEA", "Oil exports", longname)) %>% 
  mutate(source   = ifelse(variable == "energy_oil_exports_IEA", "IEA", source)) %>% 
  mutate(unit     = ifelse(variable == "energy_oil_exports_IEA", "EJ/yr", unit)) %>% 
  mutate(variable = ifelse(variable == "energy_oil_exports_IEA", "E_OEx", variable)) %>% 
  mutate(longname = ifelse(variable == "energy_gas_exports_IEA", "Gas exports", longname)) %>% 
  mutate(source   = ifelse(variable == "energy_gas_exports_IEA", "IEA", source)) %>% 
  mutate(unit     = ifelse(variable == "energy_gas_exports_IEA", "EJ/yr", unit)) %>%
  mutate(variable = ifelse(variable == "energy_gas_exports_IEA", "E_GEx", variable)) %>% 
  mutate(longname = ifelse(variable == "ratio_ee", "Ratio of final energy over primary energy", longname)) %>% 
  mutate(source   = ifelse(variable == "ratio_ee", "IEA", source)) %>% 
  mutate(unit     = ifelse(variable == "ratio_ee", "Unitless", unit)) %>% 
  mutate(variable = ifelse(variable == "ratio_ee", "EE", variable)) %>% 
  mutate(longname = ifelse(variable == "ratio_ef", "Ratio of Emission over primary energy", longname)) %>% 
  mutate(source   = ifelse(variable == "ratio_ef", "IEA", source)) %>% 
  mutate(unit     = ifelse(variable == "ratio_ef", "Tg(CO2)/GJ", unit)) %>% 
  mutate(variable = ifelse(variable == "ratio_ef", "EF", variable)) %>% 
  mutate(longname = ifelse(variable == "gdp_ppp_PWT_pc", "Gross Domestic Product per capita", longname)) %>% 
  mutate(source   = ifelse(variable == "gdp_ppp_PWT_pc", "Penn World Tables", source)) %>% 
  mutate(unit     = ifelse(variable == "gdp_ppp_PWT_pc", "PPP", unit)) %>% 
  mutate(variable = ifelse(variable == "gdp_ppp_PWT_pc", "GDPpc", variable)) %>% 
  mutate(longname = ifelse(variable == "co2_terr_CDIAC_pc", "CO2 emissions per capita", longname)) %>% 
  mutate(source   = ifelse(variable == "co2_terr_CDIAC_pc", "CDIAC", source)) %>% 
  mutate(unit     = ifelse(variable == "co2_terr_CDIAC_pc", "Tg(CO2)/yr", unit)) %>% 
  mutate(variable = ifelse(variable == "co2_terr_CDIAC_pc", "CO2pc", variable)) %>% 
  mutate(longname = ifelse(variable == "co2_terr_GCB_pc", "CO2 emissions per capita", longname)) %>% 
  mutate(source   = ifelse(variable == "co2_terr_GCB_pc", "GCP", source)) %>% 
  mutate(unit     = ifelse(variable == "co2_terr_GCB_pc", "Tg(CO2)/yr", unit)) %>% 
  mutate(variable = ifelse(variable == "co2_terr_GCB_pc", "CO2pc", variable)) %>%
  mutate(longname = ifelse(variable == "co2_cons_GCB_pc", "CO2 emissions from consumption per capita", longname)) %>% 
  mutate(source   = ifelse(variable == "co2_cons_GCB_pc", "GCP", source)) %>% 
  mutate(unit     = ifelse(variable == "co2_cons_GCB_pc", "Tg(CO2)/pers/yr", unit)) %>% 
  mutate(variable = ifelse(variable == "co2_cons_GCB_pc", "CO2pc", variable)) %>%
  # mutate(longname = ifelse(variable == "energy_tpes_IEA_pc", "Consumption of total primary energy per capita", longname)) %>% 
  # mutate(source   = ifelse(variable == "energy_tpes_IEA_pc", "IEA", source)) %>% 
  # mutate(unit     = ifelse(variable == "energy_tpes_IEA_pc", "EJ/pers/yr", unit)) %>% 
  # mutate(variable = ifelse(variable == "energy_tpes_IEA_pc", "EnPripc", variable)) %>% 
  # mutate(longname = ifelse(variable == "energy_tfec_IEA_pc", "Consumption of total final energy per capita", longname)) %>% 
  # mutate(source   = ifelse(variable == "energy_tfec_IEA_pc", "IEA", source)) %>% 
  # mutate(unit     = ifelse(variable == "energy_tfec_IEA_pc", "EJ/pers/yr", unit)) %>%
  # mutate(variable = ifelse(variable == "energy_tfec_IEA_pc", "EnFinpc", variable)) %>% 
  mutate(country  = paste(country)) %>% 
  select(country, year, variable, longname, source, unit, value)

country_nameMapping <- c(
  "British Virgin Islands"                     = "Virgin Islands (British)",
  "Côte d'Ivoire"                              = "Cote d'Ivoire",
  "Democratic Republic of the Congo"           = "Congo (Democratic Republic of the)",
  "China (Hong Kong SAR)"                      = "Hong Kong",
  "China (Macau SAR)"                          = "Macao",
  "Czech Republic"                             = "Czechia",
  "Dem. People's Republic of Korea"            = "Korea (Democratic People's Republic of)",
  "Faeroe Islands"                             = "",
  "Micronesia (Fed. States of)"                = "Micronesia (Federated States of)",
  "Republic of Korea"                          = "Korea (Republic of)",
  "Republic of Moldova"                        = "Moldova (Republic of)",
  "Réunion"                                    = "Reunion",
  "Saint Helena"                               = "Saint Helena, Ascension and Tristan da Cunha",
  "TFYR Macedonia"                             = "Macedonia (the former Yugoslav Republic of)",
  "United Kingdom"                             = "United Kingdom of Great Britain and Northern Ireland",
  "United Republic of Tanzania"                = "Tanzania, United Republic of",
  "United States Virgin Islands"               = "Virgin Islands (U.S.)",
  "Wallis and Futuna Islands"                  = "Wallis and Futuna")

p_data = p_data %>% 
  rename_countries(country_nameMapping, countries)

v_data = rbind(v_data,
               p_data %>% 
                 left_join(countries, 
                           by=c("country")) %>% 
                 select(country, iso, year, variable, longname, source, unit, value))

# Set all non finite values to NA
v_data$value[which(is.nan(v_data$value))]     = NA
v_data$value[which(!is.finite(v_data$value))] = NA

p_data = v_data


#---------------------------------------------
# SAVE DATA
#---------------------------------------------
save(p_data, file=u_outRData)
write.csv(p_data, file = paste0(substr(u_outRData, 1, nchar(u_outRData)-5), "csv"))
