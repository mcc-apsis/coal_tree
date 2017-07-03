library(MASS)
library(quantreg)
library(robust)

results <- list()

# Define model
u_model <- E_CC ~ E_CP + E_CIm + E_CEx + GDPpc + P + EE + GDP_Ind + GDP_Ser + GDP_Tra

results[["model"]] <- u_model

# Compute residuals
res <- MASS::rlm(u_model, data=v_data[[1]])
residuals <- res$residuals

# Split residuals at quartiles and compute contingency table for al n vars
results[["residuals"]] <- residuals
for (kv in attr(terms(u_model), "term.labels")) {
  
  results[[kv]] <- list()
  
  tmp <- v_data[[1]][[kv]][as.numeric(names(residuals))]
  
  qs <- quantile(tmp, c(0.25, 0.5, 0.75))
  
  results[[kv]][["quantiles"]] <- qs
  
  tbl <- matrix(0, nrow=2, ncol=4)
  tbl[1, 1] <- length(which(as.numeric(residuals)[which(tmp < qs[1])] >= 0))
  tbl[1, 2] <- length(which(as.numeric(residuals)[which(tmp >= qs[1] & tmp < qs[2])] >= 0))
  tbl[1, 3] <- length(which(as.numeric(residuals)[which(tmp >= qs[2] & tmp < qs[2])] >= 0))
  tbl[1, 4] <- length(which(as.numeric(residuals)[which(tmp >= qs[3])] >= 0))
  tbl[2, 1] <- length(which(as.numeric(residuals)[which(tmp < qs[1])] < 0))
  tbl[2, 2] <- length(which(as.numeric(residuals)[which(tmp >= qs[1] & tmp < qs[2])] < 0))
  tbl[2, 3] <- length(which(as.numeric(residuals)[which(tmp >= qs[2] & tmp < qs[3])] < 0))
  tbl[2, 4] <- length(which(as.numeric(residuals)[which(tmp >= qs[3])] < 0))
  
  results[[kv]][["ctable"]] <- tbl
  
  # Compute chi-square compare to chi-square 2/3
  results[[kv]][["chisq"]] <- chisq.test(tbl)                       
                      
}



# Statistical version
u_samplingCases = 60
results_stats <- list()
for (k in 1:u_samplingCases) {
  
  results_stats[[k]] <- list()
  
  # Sample data
  tmp_data <- v_data[[1]] %>% 
    sample_n(nrow(v_data[[1]])-1)
  
  results_stats[[k]][["rm.obs"]] <- paste0(v_data[[1]]$iso,"-",v_data[[1]]$year)[which(!paste0(v_data[[1]]$iso,"-",v_data[[1]]$year) %in% paste0(tmp_data$iso,"-",tmp_data$year))]
  
  # Compute residuals
  res <- MASS::rlm(u_model, data=tmp_data)
  #res <- quantreg::
  #res <- robust::glmRob(u_model, data=tmp_data)
  residuals <- res$residuals
  
  # Split residuals at quartiles and compute contingency table for al n vars
  results_stats[[k]][["residuals"]] <- residuals
  results_stats[[k]][["variables"]] <- list()
  for (kv in attr(terms(u_model), "term.labels")) {
    
    results_stats[[k]][["variables"]][[kv]] <- list()
    
    # Filter out NA data
    tmp <- tmp_data[[kv]][as.numeric(names(residuals))]
    
    # Compute quantiles
    qs <- quantile(tmp, c(0.25, 0.5, 0.75))
    results_stats[[k]][["variables"]][[kv]][["quantiles"]] <- qs
    
    # Generate contingency table
    tbl <- matrix(0, nrow=2, ncol=4)
    # Positive residuals row
    tbl[1, 1] <- length(which(as.numeric(residuals)[which(tmp <  qs[1])]               >= 0)) #Q1
    tbl[1, 2] <- length(which(as.numeric(residuals)[which(tmp >= qs[1] & tmp < qs[2])] >= 0)) #Q2
    tbl[1, 3] <- length(which(as.numeric(residuals)[which(tmp >= qs[2] & tmp < qs[2])] >= 0)) #Q3
    tbl[1, 4] <- length(which(as.numeric(residuals)[which(tmp >= qs[3])]               >= 0)) #Q4
    # Negative residuals row
    tbl[2, 1] <- length(which(as.numeric(residuals)[which(tmp <  qs[1])]               < 0)) #Q1
    tbl[2, 2] <- length(which(as.numeric(residuals)[which(tmp >= qs[1] & tmp < qs[2])] < 0)) #Q2
    tbl[2, 3] <- length(which(as.numeric(residuals)[which(tmp >= qs[2] & tmp < qs[3])] < 0)) #Q3
    tbl[2, 4] <- length(which(as.numeric(residuals)[which(tmp >= qs[3])]               < 0)) #Q4
    
    results_stats[[k]][["variables"]][[kv]][["ctable"]] <- tbl
    
    # Compute chi-square compare to chi-square 2/3
    results_stats[[k]][["variables"]][[kv]][["chisq"]] <- chisq.test(tbl)                       
    
  }

}

data_plot <- do.call("rbind", 
  lapply(1:length(results_stats), 
         function(x) {
           do.call("rbind", 
             lapply(1:length(results_stats[[x]]$variables), 
                    function(y) {
                      data.frame(case=x, 
                                 variable=names(results_stats[[x]]$variables)[y], 
                                 pval=as.numeric(results_stats[[x]]$variables[[y]]$chisq$p.value))
                      }
                    )
           )
           }))



  
  

