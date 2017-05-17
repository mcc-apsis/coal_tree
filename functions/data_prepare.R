filter_NA <- function(i_data, i_vardef, MISSINGRATIO=0.0) {
  # Select independent and dependent variables of current model (x means exclude)
  varnames <- paste((i_vardef %>% filter(type != "x"))$variable)
  dvar     <- paste((i_vardef %>% filter(type == "d"))$variable)
  
  # Remove all NAs in dependent variable
  i_data <- i_data[which(!is.na(i_data[,dvar])),]
  
  # Count number of NA values for each couple (iso, variable)
  data_NA_country_variable <- i_data %>% 
    gather(variable, value, -iso, -year) %>%        # Switch to long format
    inner_join(i_vardef, by = "variable") %>%       # Join with variable definition table
    filter(type != "x") %>%                         # Remove excluded variables
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
  
  # Select countries with the minimum number of NAs
  iso_minNA <- paste(data_NA_country$iso[which(data_NA_country$value/max(data_NA_country$value) >= (1- MISSINGRATIO))])
  
  out <- i_data %>% filter(iso %in% iso_minNA)
  
  return(out)
}

filter_X <- function(i_data, i_vardef) {
  
  # Get selected variable names 
  varnames <- paste(i_vardef$variable[which(i_vardef$type != "x")])

  # Select variables
  i_data <- i_data[,c("iso","year",varnames)]
  
  return(i_data)
}

filter_zeros <- function(i_data, i_vardef, eps=0.00001) {
  
  # Get selected dependent variable name
  dvarname <- paste(i_vardef$variable[which(i_vardef$type == "d")])
  
  # Select non 0s countries
  tmp <- i_data[,c("iso","year",dvarname)] %>% 
    rename_("value"=dvarname) %>% 
    group_by(iso) %>% 
    summarise(keep = (!all(value == 0))) %>% 
    ungroup() %>% 
    filter(keep) %>% 
    select(iso)
  
  i_data <- i_data %>% 
    filter(iso %in% tmp$iso) 

  tmp <- i_data %>% 
    gather(variable,value,-iso,-year) %>% 
    group_by(iso,variable) %>%
    filter(value != 0) %>%  
    summarise(min_val = min(value)) %>% 
    ungroup()

  i_data <- i_data %>% 
    gather(variable,value,-iso,-year) %>% 
    left_join(tmp,
              by=c("iso","variable")) %>% 
    mutate(value=ifelse(value == 0, min_val*1e-3, value)) %>% 
    select(-min_val) %>%
    mutate(value = ifelse(!is.finite(value), eps, value)) %>% 
    spread(variable,value)

  return(i_data)
}

rescaleVariable <- function(i_data, i_vardef, VERBOSE=FALSE) {
  # Get names of numerical variables
  numVarnames <- paste((i_vardef %>% filter(type %in% c("d", "n", "f", "s"), factor != 1))$variable)
  
  # Loop over variables and apply factor
  for (kvar in numVarnames) {
    tmp_factor <- as.numeric(i_vardef$factor[which(i_vardef$variable == kvar)])
    if (VERBOSE) cat("    - ", kvar, " <- ", kvar, "*", paste(tmp_factor), "\n")
    i_data[,kvar] <- i_data[,kvar] * tmp_factor
  }
  
  return(i_data)
}

transformVariable <- function(i_data, i_vardef, VERBOSE=FALSE) {
  # Get names of numerical variables
  numVarnames <- paste((i_vardef %>% filter(type %in% c("d", "n", "f", "s")))$variable)
  
  # Loop over variables and apply transforming function
  for (kvar in numVarnames) {
    # Logarithm
    if (i_vardef$transform[which(i_vardef$variable == kvar)] == "log") {
      if (VERBOSE) cat("    - ", kvar, " <- log(", kvar, ")\n")
      # Apply transformation
      i_data[,kvar] <- log(i_data[,kvar])
      # Switch infinite values to NAs
      i_data[which(is.infinite(i_data[,kvar])),kvar] <- NA
    }
    # Other functions (TODO)...
  }
  
  return(i_data)
}

applyFirstDiff <- function(i_data, i_vardef) {
  # Get names of numerical variables
  numVarnames <- paste((i_vardef %>% filter(type %in% c("d", "n", "f", "s")))$variable)
  
  # Loop over variables and apply transforming function
  for (kvar in numVarnames) {
    i_data <- i_data %>% 
      group_by(iso) %>% 
      arrange(year) %>% 
      firstdiff(kvar) %>% 
      ungroup()
  }
  
  return(i_data)
}

getVariableMeans <- function(i_data, i_vardef, GROUPBY="iso", VERBOSE=FALSE) {
  # Get names of numerical variables
  numVarnames <- paste((i_vardef %>% filter(type %in% c("d", "n", "f", "s")))$variable)

  out <- i_data %>% 
    gather(variable, value, -iso, -year) %>% 
    filter(variable %in% numVarnames) %>% 
    group_by(iso, variable) %>%
    summarise(mean=mean(value, na.rm=TRUE)) %>% 
    ungroup()
  
  return(out)
}

demeanVariable <- function(i_data, i_vardef, GROUPBY="iso", VERBOSE=FALSE) {
  # Get names of numerical variables
  numVarnames <- paste((i_vardef %>% filter(type %in% c("d", "n", "f", "s")))$variable)
  
  # Loop over variables and demean
  for (kvar in numVarnames) {
    if (VERBOSE) cat("    - ", kvar, "<-", kvar, "- mean(", kvar, ")\n")
    i_data <- i_data %>% 
      group_by_(GROUPBY) %>% 
      demean(kvar) %>% 
      ungroup()
  }
  
  return(i_data)
}

get_renamedVariables <- function(i_vardef, ALL=FALSE, VERBOSE=FALSE) {
  # Initialise
  v_variableRenaming = data.frame()
  
  # Get names of numerical variables
  numVarnames <- paste((i_vardef %>% filter(type %in% c("d", "n", "f", "s")))$variable)
  
  # Loop over variables and apply transforming function
  for (kvar in numVarnames) {
    # Logarithm
    if (i_vardef$transform[which(i_vardef$variable == kvar)] == "log") {
      if (VERBOSE) cat("    - ", kvar, " -> ", paste0("log_",kvar), "\n")
      # Rename variable
      v_variableRenaming <- rbind(v_variableRenaming,
                                  data.frame(old=kvar, new=paste0("log_",kvar)))
    }
    # Other functions (TODO)...
  }
  
  if (ALL) {
    for (kvar in i_vardef$variable) {
      if (!kvar %in% v_variableRenaming$old) {
        v_variableRenaming <- rbind(v_variableRenaming,
                                    data.frame(old=kvar, new=kvar))
      }
    }
  }
  
  return(v_variableRenaming)
}

renameVariable <- function(i_data, i_vardef, VERBOSE=FALSE) {
  
  v_variableRenaming <- get_renamedVariables(i_vardef, VERBOSE=VERBOSE)
  
  names(i_data) <- paste(sapply(names(i_data), function(x) {
    ifelse(length(which(v_variableRenaming$old == x)) != 0,                  
           paste(v_variableRenaming$new[which(v_variableRenaming$old == x)]),
           x)
  }))
  
  return(i_data)
}

process_categoricalVariables <- function(i_data, i_vardef, VERBOSE=FALSE) {
  # Get names of numerical variables
  catVarnames <- paste((i_vardef %>% filter(type %in% c("c")))$variable)
  
  depvar <- paste((i_vardef %>% filter(type %in% c("d")))$variable)
  
  # Loop over categorical variables
  for (kvar in catVarnames) {
    if (VERBOSE) cat("    - spread ", kvar, "\n")
    
    tmp_year <- 2010
    i_data <- rbind(
      i_data %>% 
        filter(variable != kvar),
      left_join(
        i_data %>% 
          filter(variable == depvar) %>% 
          select(year, iso),
        tmp_data <- i_data %>% 
          filter(variable == kvar, year == tmp_year) %>% 
          select(-year),
        by=c("iso")))
  }
  
  return(i_data)
  
}

data_prepare <- function(i_data, i_vardef, period_start, period_end, MISSINGRATIO=0.0, REMOVE_ZEROS=FALSE, FIRSTDIFF=FALSE, DEMEAN=TRUE, VERBOSE=FALSE) {
  
  # Filter years
  if (VERBOSE) cat("> Filtering  years...\n")
  i_data %<>% filter(year >= period_start, year <= period_end)
  
  # Select variables which have not been excluded
  if (VERBOSE) cat("> Removing excluded variable from data (keep iso and year)...\n")
  i_data %<>% filter_X(i_vardef)
  
  # Select countries with the minimum number of NAs
  if (VERBOSE) cat("> Selecting countries with minimum number of NAs...\n")
  i_data %<>% filter_NA(i_vardef, MISSINGRATIO)
  
  # Select countries in which the dependent variable has at least 1 non-0 value
  if (REMOVE_ZEROS) {
    if (VERBOSE) cat("> Selecting countries with non 0s for all years...\n")
    i_data %<>% filter_zeros(i_vardef)
  }
  
  # Rescale variables
  if (VERBOSE) cat("> Rescaling numerical variables...\n")
  i_data %<>% rescaleVariable(i_vardef, VERBOSE=VERBOSE)
  
  # Transform variables (e.g. log)
  if (VERBOSE) cat("> Transforming numerical variables...\n")
  i_data %<>% transformVariable(i_vardef, VERBOSE=VERBOSE)
  
  # Apply first differences to variables
  if (FIRSTDIFF) {
    if (VERBOSE) cat("> Applying first differences to variables...\n")
    i_data %<>% applyFirstDiff(i_vardef)
  }
  
  # Demean variables
  if (DEMEAN) {
    if (VERBOSE) cat("> Demeaning numerical variables...\n")
    data_mean <- i_data %>% getVariableMeans(i_vardef, VERBOSE=VERBOSE)
    i_data %<>% demeanVariable(i_vardef, VERBOSE=VERBOSE)
  } else {
    data_mean <- NULL
  }
  
  
  # Rename transformed variables in data
  if (VERBOSE) cat("> Renaming transformed variables...\n")
  i_data %<>% renameVariable(i_vardef, VERBOSE=VERBOSE)
  
  return(list("data"=i_data, "datamean"=data_mean))
}