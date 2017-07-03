set.seed(123456)
#======== USER SECTION ==================================================
# Data filtering options (countries and years)
u_iso <- c("CHN", "USA", "DEU", "IND", "JPN", "POL", "GBR", "ZAF", "FRA", "KOR", "TUR", "CAN", "AUS", "BEL", "ITA", "BRA", "ESP", "IDN", # 18
           "VNM", "THA", "HUN", "ROU", "AUT", "BGR", "NLD", "PAK", "MEX", "COL", "SWE", "FIN", "ZWE", "NOR", "NZL", "DNK", "GRC", "PHL", # 36
           "MYS", "CHL", "CHE", "PRT", "EGY", "PER", "IRN", "ALB", "HKG", "ARG", "BGD", "MAR", "ZMB", "ARE", "DZA", "MMR", "NPL", "VEN", # 54
           "COD", "KEN", "MOZ", "LBN", "DOM", "NGA", "HND", "TUN", "SEN", "PAN", "JAM", "CYP", "LKA", "ETH", "MUS", "JOR", "CRI", "ISR", # 72
           "SGP", "URY", "TZA", "BOL", "HTI", "SYR", "GTM", "SLV")
u_period <- c(1971, 2012)

# Data processing options
u_missingRatio   <- 0.20  # Maximum ratio of missing values per country (this will remove countries from the list above)
u_eps            <- 1e-3  # Replacement value for 0 values in data
u_log            <- TRUE  # Log-transform data
u_demean         <- TRUE  # time-demean data

# Data sampling options
u_samplingCases  <- 2      # Number of sampling cases
u_sampleFrac     <- 0.95   # Remove 5% of the observations

# Model formulation
# Dependent variables  : E_CC, E_CCGDP, E_CCP
# Independent variables: E_CP, E_CPGDP, E_CIm, E_CImGDP, E_CEx, E_CExGDP, GDP, GDPpc, P, GDP_Ag, GDP_Ind, GDP_Ser, 
#                        GDP_Tra, URB, LEx, EE ...
u_model  <- E_CCGDP ~ E_CE + E_CRe + GDPpc + P + EE + GDP_Ind + GDP_Ser + GDP_Tra

# Tree options
u_minsplit  <- 20   # Minimum number of splits
u_minbucket <- 3    # Minimum number of observations in a terminal node
u_maxdepth  <- 9    # Maximum tree branch depth


#======== INITIALISE ==================================================
# Load libraries
tryCatch(library(dplyr),   error = function(e) {install.packages("dplyr")})
tryCatch(library(tidyr),   error = function(e) {install.packages("tidyr")})
tryCatch(library(ggplot2), error = function(e) {install.packages("ggplot2")})
tryCatch(library(party),   error = function(e) {install.packages("party")})

# Function to filter out NAs in data
filter_NAs <- function(i_data, i_dvar, MISSINGRATIO=0.0) {
  # Select independent and dependent variables of current model (x means exclude)
  varnames <- names(i_data)[which(!names(i_data) %in% c("iso", "year", i_dvar))]
  
  # Remove all NAs in dependent variable
  i_data <- i_data[which(!is.na(i_data[,i_dvar])),]
  
  # Count number of NA values for each couple (iso, variable)
  data_NA_country_variable <- i_data %>% 
    gather(variable, value, -iso, -year) %>%        # Switch to long format
    mutate(variable = factor(variable, 
                             levels=varnames, 
                             ordered=TRUE)) %>%     # Re-factor variables
    group_by(iso,variable) %>%                      # For each couple (iso, variable)
    mutate(count=ifelse(is.na(value), 0, 1)) %>%    #  . count number of NA values
    summarise(value=sum(count)) %>%                 #  . and sum them up
    ungroup() %>%                                   #
    spread(variable,value) %>%                      # Switch back to wide format
    mutate(en = max(i_data$year) - min(i_data$year) +1)  # Expected number of non NA values
  
  # Count number of NA values for each iso
  data_NA_country <- data_NA_country_variable %>% 
    select(-en) %>% 
    gather(variable,value,-iso) %>% 
    group_by(iso) %>% 
    summarize(value=sum(value)) %>% 
    ungroup() %>%                      
    mutate(en = (max(i_data$year) - min(i_data$year) +1)*length(varnames))
  
  print(data_NA_country %>% arrange(desc(value)) %>% mutate(ratio = value/max(value)) %>% mutate(selected = ratio >= (1- MISSINGRATIO)))
  
  # Select countries with the minimum number of NAs
  iso_minNA <- paste(data_NA_country$iso[which(data_NA_country$value/max(data_NA_country$value) >= (1- MISSINGRATIO))])
  cat(paste0("Selected countries (", length(iso_minNA),"): ", paste(iso_minNA, collapse=", "), "\n"))
  
  out <- i_data %>% filter(iso %in% iso_minNA)
  
  return(out)
}

# Function to process the data (filter out NAs, log-transformation, time-demeaning ...)
process_data <- function(i_model, i_period, i_iso, i_missingRatio, i_log=TRUE, i_demean=TRUE) {
  tmp_data <- data_coal %>% 
    filter(variable %in% c(paste(i_model[[2]]), attr(terms(i_model), "term.labels"))) %>% 
    filter(iso %in% i_iso) %>% 
    filter(year >= i_period[1], year <= i_period[2])
  
  # Filter NAs
  tmp_data <- tmp_data %>%
    spread(variable, value) %>% 
    filter_NAs(paste(u_model[[2]]), MISSINGRATIO=i_missingRatio) %>% 
    gather(variable, value, -iso, -year)
  
  # Log-transform variables
  if (i_log) {
    tmp_data$value[which(tmp_data$value == 0)] <- u_eps
    tmp_data <- tmp_data %>% 
      mutate(value = ifelse(!variable %in% c("iso", "year"), log(value), value))
    #tmp_data$value[which(tmp_data$value == log(u_eps))] <- 0.0
  }
  
  # Demean variables
  if (i_demean) {
    tmp_data <- tmp_data %>% 
      group_by(iso, variable) %>% 
      mutate(value = ifelse(!variable %in% c("iso", "year"), value -mean(value, na.rm=TRUE), value)) %>% 
      ungroup()
  }
  
  tmp_data <- tmp_data %>% 
    mutate(value=ifelse(!is.finite(value), NA, value)) %>% 
    spread(variable, value)
  
  TotalObs <- dim(tmp_data)[1]
  
  return(tmp_data)
}

# Load data (if required)
if (length(which(ls() == "data_coal")) == 0) load("coaldata.RData")


#======== PROCESS DATA ==================================================
tmp_data <- process_data(u_model, u_period, u_iso, u_missingRatio)

# Generate samples (removing XX% of observations)
v_data <- list()
for (ksc in 1:u_samplingCases) {
  v_data[[ksc]] <- tmp_data %>% 
    sample_frac(u_sampleFrac) 
}


#======== GENERATE TREE ==================================================
# Conditional inference trees estimate a regression relationship by binary 
# recursive partitioning in a conditional inference framework. 
# Roughly, the algorithm works as follows: 
#   1) Test the global null hypothesis of independence between any of the 
#      input variables and the response (which may be multivariate as well). 
#      Stop if this hypothesis cannot be rejected. Otherwise select the 
#      input variable with strongest association to the resonse. This 
#      association is measured by a p-value corresponding to a test for the 
#      partial null hypothesis of a single input variable and the response. 
#   2) Implement a binary split in the selected input variable. 
#   3) Recursively repeate steps 1) and 2).

v_ctree <- list()
for(k in 1:u_samplingCases) {
  
  v_ctree[[k]] <- party::ctree(
    u_model, 
    data     = v_data[[k]], 
    subset   = rep(TRUE, dim(v_data[[k]])[1]), 
    controls = party::ctree_control(teststat       = "quad",        # Statistical test: quad, max
                                    testtype       = "Bonferroni",  # Statistical type: Bonferroni, MonteCarlo, Univariate, Teststatistic
                                    mincriterion   = 0.95,          # The p-value must be smaller than 0.05 in order to split this node
                                    minsplit       = u_minsplit,    #
                                    minbucket      = u_minbucket,   #
                                    maxdepth       = u_maxdepth,    #
                                    stump          = FALSE,         #
                                    nresample      = 9999,          #
                                    maxsurrogate   = 0,             #
                                    mtry           = 0,             #
                                    savesplitstats = TRUE,          #
                                    remove_weights = FALSE          #
    )) 
  
}


#======== PLOT RESULTS ===================================================
# Plot data for the main coal countries
p <- ggplot(v_data[[1]] %>% filter(iso %in% u_iso[1:6]) %>% gather(variable, value, -iso, -year)) +
  geom_line(aes(x=year, y=value)) +
  facet_grid(variable~iso, scales="free_y")
print(p)

# Plot trees
for(k in 1:u_samplingCases) {
  plot(v_ctree[[k]]) # By default boxplot appears below temrinal nodes (other options: density plot, histogram)
}