# Load following package first
#
#-------------------------start of main function----------------------------------------------------
REEMctree <- function (formula, data, random, subset = NULL, initialRandomEffects = rep(0,TotalObs), ErrorTolerance = 0.001, MaxIterations = 1000, 
                       verbose = FALSE, lme.control = lmeControl(returnObject = TRUE), ctree.control = party::ctree_control(mincriterion = 0.95),
                       method = "REML", correlation = NULL) 
{
  require(REEMtree)
  require(party)

  # Get total number of observations
  TotalObs <- dim(data)[1]
  
  # Data subset
  if (identical(subset, NULL)) {
    subs <- rep(TRUE, dim(data)[1])
  } else {
    subs <- subset
  }
  
  # Get predictors from formula
  Predictors <- paste(attr(terms(formula), "term.labels"), 
                      collapse = "+")
  
  # Get dependent variable
  TargetName <- formula[[2]]
  
  # If multiple dependent variables
  if (length(TargetName) > 1) 
    TargetName <- TargetName[3]
  
  # Print dependent variable
  if (verbose) 
    print(paste("Target variable: ", TargetName))
  
  # Depenpent variable data
  Target <- data[, toString(TargetName)]
  
  ContinueCondition         <- TRUE                            # Convergence confition flag
  iterations                <- 0                               # Iteration counter
  AdjustedTarget            <- Target - initialRandomEffects   # Adjusted target data (substracting random effects)
  oldlik                    <- -Inf                            # Old likelihood
  newdata                   <- data                            # Data
  newdata[, "SubsetVector"] <- subs                            # Data subset
  
  # Loop until convergence
  while (ContinueCondition) {
    
    # Update data
    newdata[, "AdjustedTarget"] <- AdjustedTarget
    
    # Update iteration counter
    iterations                  <- iterations + 1
    if (verbose) 
      print(paste0("Iteration: ", iterations))
    
    # Generate a regression tree with C-tree using adjusted target data
    tree <- party::ctree(
      formula(paste(c("AdjustedTarget", Predictors), collapse = "~")), 
      data     = newdata, 
      subset   = subs, 
      controls = ctree.control)  
    
    # Print tree
    if (verbose) 
      print(tree)
    
    # Allocate node IDs to data
    newdata[, "nodeInd"]     <- 0
    newdata[subs, "nodeInd"] <- party::where(tree)
    
    # Compute linear mixed-effects model on terminal nodes
    if (min(party::where(tree)) == max(party::where(tree))) { #it doesn't split on root
      # formula: log_E_CC ~ 1
      lmefit <- lme(formula(paste(c(toString(TargetName), 1), collapse = "~")), 
                    data        = newdata, 
                    random      = random, 
                    subset      = SubsetVector, 
                    method      = method, 
                    control     = lme.control, 
                    correlation = correlation)
    } else {
      # formula: log_E_CC ~ as.factor(nodeInd)
      lmefit <- lme(formula(paste(c(toString(TargetName), "as.factor(nodeInd)"), collapse = "~")), 
                    data        = newdata, 
                    random      = random, 
                    subset      = SubsetVector, 
                    method      = method, 
                    control     = lme.control, 
                    correlation = correlation)
    }
    
    # Print model stats
    if (verbose) {
      print(lmefit)
      print(paste("Estimated Error Variance = ", lmefit$sigma))
      print("Estimated Random Effects Variance = ")
      print(as.matrix(lmefit$modelStruct$reStruct[[1]]) * lmefit$sigma^2)
    }
    
    # Get new log-likelihood from new fitting model
    newlik <- logLik(lmefit)
    
    # Print new log-likelihood
    if (verbose) 
      print(paste("Log likelihood: ", newlik))
    
    # Compute convergence criterion
    ContinueCondition <- (newlik - oldlik > ErrorTolerance & 
                            iterations < MaxIterations)
    
    # Update and save new log-likelihood
    oldlik <- newlik
    
    # Adjust data (Y - Zb)
    AllEffects            <- lmefit$residuals[, 1] - lmefit$residuals[, dim(lmefit$residuals)[2]]    
    AdjustedTarget[subs]  <- Target[subs] - AllEffects
  }
  
  # Compute residuals
  residuals                <- rep(NA, length = dim(Target)[1])
  residuals[subs]          <- Target[subs] - predict(lmefit)
  attr(residuals, "label") <- NULL 
  
  # Save results in REEMctree object
  result <- list(Tree           = tree, 
                 EffectModel    = lmefit, 
                 RandomEffects  = ranef(lmefit), 
                 BetweenMatrix  = as.matrix(lmefit$modelStruct$reStruct[[1]]) * lmefit$sigma^2, 
                 ErrorVariance  = lmefit$sigma^2, 
                 data           = data, 
                 logLik         = newlik, 
                 IterationsUsed = iterations, 
                 Formula        = formula, 
                 Random         = random, 
                 Subset         = subs, 
                 ErrorTolerance = ErrorTolerance, 
                 correlation    = correlation, 
                 residuals      = residuals, 
                 method         = method, 
                 lme.control    = lme.control, 
                 ctree.control  = ctree.control)
  class(result) <- "REEMctree"
  
  return(result)
}
#-------------------------end of main function----------------------------------------------------
#--------------------------------------------------------------------------------------------------
# Example to use unbiased RE-EM tree
# library(REEMtree) # Only needed to access data set
# library(party)
# data(simpleREEMdata)
# REEM.ctree.result<-REEMctree(Y~D+t+X, data=simpleREEMdata, random=~1|ID)
# plot(REEM.ctree.result$Tree)

### Some functions in the original RE-EM tree may be lost or need different treatment such as using the predict() function
### to predict the fixed effect of testing data from a fitted tree. In particular, if a correlation structure other than
### independence is assumed for errors within individuals, the predicted response values at the terminal nodes of the tree
### will not be correct, and need to be obtained from the associated mixed model fit; that is,
###          unique(cbind(where(REEM.ctree.result$Tree), predict(REEM.ctree.result$Tree)))
### will NOT give the correct responses at the terminal nodes, but
###          unique(cbind(where(REEM.ctree.result$Tree), predict(REEM.ctree.result$EffectModel, level = 0)))
### will. Note that this means that the estimated responses at the terminal nodes when the tree is plotted will be (slightly)
### incorrect as well.


