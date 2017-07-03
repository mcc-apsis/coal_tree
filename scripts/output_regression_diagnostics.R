#' ---
#' title: "Regression diagnostics"
#' author: "Jerome Hilaire"
#' date: "June 6th, 2017"
#' ---
#' 

#' #Initialisation and data processing
#' This hidden part of the code loads the necessary libraries and performs a simple LM
#+ Initialise, echo=FALSE, warning=FALSE, message=FALSE
# Taken from: http://www.statmethods.net/stats/rdiagnostics.html
suppressWarnings(suppressPackageStartupMessages(library(ggplot2)))
suppressWarnings(suppressPackageStartupMessages(library(dplyr)))
suppressWarnings(suppressPackageStartupMessages(library(tidyr)))
suppressWarnings(suppressPackageStartupMessages(library(car)))
suppressWarnings(suppressPackageStartupMessages(library(gvlma)))
suppressWarnings(suppressPackageStartupMessages(library(MASS)))
library(knitr)
library(markdown)
library(rmarkdown)
# Save data locally
data  <- v_dataShortProc$data
# Identify dependent and independent variables
dvar  <- names(v_dataShortProc$data)[grep("E_CC", names(v_dataShortProc$data))]
idvar <- names(v_dataShortProc$data)[which(!names(v_dataShortProc$data) %in% c("iso", "year", "log_E_CC"))] 
# Compute (multi)linear regression
fit <- lm(
  as.formula(paste(dvar, " ~ ", paste(idvar, collapse=" + "))), 
  data=data)


#' #Searching for outliers
#' **Bonferroni Outlier Test**. This test reports the Bonferroni p-values for Studentized residuals in linear and generalized linear models, based on a t-test for linear models and normal-distribution test for generalized linear models.
#+ Assessing Outliers 1, echo=FALSE
out1 <- outlierTest(fit, labels = paste0(data$iso, "-", data$year)) # Bonferonni p-value for most extreme obs
print(out1)
cat(paste0("Outliers (based on Bonf. p-value): ", paste0(names(out1$rstudent), collapse=", "), "\n"))

#' **QQ-plot**. This plot shows empirical quantiles of a variable, or of studentized residuals from a linear model, against theoretical quantiles of a comparison distribution.
#+ Assessing Outliers 2, echo=FALSE
out2 <- qqPlot(fit, main="QQ Plot", id.n=20, labels = paste0(data$iso, "-", data$year)) #qq plot for studentized resid 
cat(paste0("Outliers (based on studentized residuals): ", paste0(names(out2), collapse=", "), "\n"))

#' **Cook's D plot**. This plots helps identifying influencial observations. It reports which D values are greater than 4/(n-k-1) 
#+ Checking for influential Observations, echo=FALSE
cutoff <- 4/((nrow(data)-length(fit$coefficients)-2)) 
plot(fit, which=4, cook.levels=cutoff)

#' #Cheking the distribution of studentized residuals
#' **The Studentized residuals**. Like standardized residuals, these are normalized to unit variance, but the Studentized version is fitted ignoring the current data point. (They are sometimes called jackknifed residuals).
#+ Normality of Residuals, echo=FALSE
sresid <- studres(fit) 
hist(sresid, freq=FALSE, 
     main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40) 
yfit<-dnorm(xfit) 
lines(xfit, yfit, col="red")

#' #Checking linear model assumptions
#' ##Global test of model assumptions
#' This function performs a global validation of linear models assumptions.
#' Note: Heteroskedasticity test will probably fail.
#+ Global test of model assumptions, echo=FALSE
gvmodel <- gvlma(fit, alphalevel = 0.05) 
summary(gvmodel)

#' ##Evaluating homoscedasticity
#' **Non-constant error variance test**. 
#+ Non-constant error variance test, echo=FALSE
ncvTest(fit)

#' **Spread-level plots**. These plots are used for examining the possible dependence of spread on level, or an extension of these plots to the studentized residuals from linear models.
#+ Spread-level plots, echo=FALSE
spreadLevelPlot(fit)

#' ##Evaluating Collinearity
#' **Variance inflation factors**. This function calculates variance-inflation and generalized variance-inflation factors for linear and generalized linear models.
#+ Variance inflation factors, echo=FALSE
vif(fit) 
vars <- paste0(names(sqrt(vif(fit)) > 2)[which(sqrt(vif(fit)) > 2)], collapse=", ")
if (length(vars) > 0) cat(paste0("VIF results - Potential problems with: ", vars, "\n"))

#' ##Evaluating Auto-collinearity
#' **Durbin-Watson Test**. This function computes residual autocorrelations and generalized Durbin-Watson statistics and their bootstrapped p-values.
#+ Test for Autocorrelated Errors, echo=FALSE
ace <- durbinWatsonTest(fit, max.lag=10)
print(ace)
