#=== Initialise =======================
# Taken from: http://www.statmethods.net/stats/rdiagnostics.html
library(car)
library(gvlma)
library(MASS)


#=== Get and process data =============
# Save data locally
data  <- v_dataShortProc$data

# Identify dependent and independent variables
dvar  <- names(v_dataShortProc$data)[grep("E_CC", names(v_dataShortProc$data))]
idvar <- names(v_dataShortProc$data)[which(!names(v_dataShortProc$data) %in% c("iso", "year", "log_E_CC"))] 

# Compute (multi)linear regression
fit <- lm(
  as.formula(paste(dvar, " ~ ", paste(idvar, collapse=" + "))), 
  data=data)


#=== Evaluate data =============
#=== Assessing Outliers ========
#--- Bonferroni Outlier Test -----
# Reports the Bonferroni p-values for Studentized residuals in linear and generalized linear models, 
# based on a t-test for linear models and normal-distribution test for generalized linear models.
out1 <- outlierTest(fit, labels = paste0(data$iso, "-", data$year)) # Bonferonni p-value for most extreme obs
print(out1)
paste0("Outliers (based on Bonf. p-value): ", paste0(names(out1$rstudent), collapse=", "))

#--- QQ-plot -----------------
# Plots empirical quantiles of a variable, or of studentized residuals from a linear model, 
# against theoretical quantiles of a comparison distribution.
out2 <- qqPlot(fit, main="QQ Plot", id.n=20, labels = paste0(data$iso, "-", data$year)) #qq plot for studentized resid 
paste0("Outliers (based on studentized residuals): ", paste0(names(out2), collapse=", "))

#--- Leverage plots ----------
# !! INTERACTIVE !!
# These functions display a generalization, due to Sall (1990) and Cook and Weisberg (1991), 
# of added-variable plots to multiple-df terms in a linear model. When a term has just 1 df, 
# the leverage plot is a rescaled version of the usual added-variable (partial-regression) plot.
# out3 <- leveragePlots(fit) # leverage plots

#=== Checking for influential Observations ==========
#--- Added variable plots ------
# !! INTERACTIVE !!
# These functions construct added-variable (also called partial-regression) plots 
# for linear and generalized linear models.
#cio1 <- avPlots(fit)

#--- Cook s D plot -------------
# Identify D values which are greater than 4/(n-k-1) 
cutoff <- 4/((nrow(data)-length(fit$coefficients)-2)) 
cio2   <- plot(fit, which=4, cook.levels=cutoff)

#--- Influence Plot ------------
# !! INTERACTIVE !!
# This function creates a “bubble” plot of Studentized residuals by hat values, with the areas of the circles 
# representing the observations proportional to Cook's distances. Vertical reference lines are drawn at twice 
# and three times the average hat value, horizontal reference lines at -2, 0, and 2 on the Studentized-residual 
# scale.
#influencePlot(fit,	id.method="identify", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )

#=== Normality of Residuals ====================================
#--- QQ plot for studentized resid -----------------
#qqPlot(fit, main="QQ Plot")

#--- Distribution of studentized residuals ------
sresid <- studres(fit) 
hist(sresid, freq=FALSE, 
     main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40) 
yfit<-dnorm(xfit) 
lines(xfit, yfit)

#=== Evaluate homoscedasticity (Non-constant error variance) ===========
#--- Non-constant error variance test -------------
ncvTest(fit)

#--- Spread-level plots ------
# Creates plots for examining the possible dependence of spread on level, or an extension of these plots 
# to the studentized residuals from linear models.
spreadLevelPlot(fit)

#=== Evaluate Collinearity ==========================================
#--- Variance inflation factors  ------------
# Calculates variance-inflation and generalized variance-inflation factors for 
# linear and generalized linear models.
vif(fit) 
sqrt(vif(fit)) > 2 # problem?
paste0("VIF results - Potential problems with: ", paste0(names(sqrt(vif(fit)) > 2)[which(sqrt(vif(fit)) > 2)], collapse=", "))

#=== Evaluate Nonlinearity =========================================
#--- Component+Residual (Partial Residual) Plots -------------
# !! INTERACTIVE !!
# These functions construct component+residual plots (also called partial-residual plots) 
# for linear and generalized linear models.
#crPlots(fit)

#--- Ceres plots  -----------------
# !! INTERACTIVE !!
# These functions draw Ceres plots for linear and generalized linear models.
# ceresPlots(fit)

#=== Test for Autocorrelated Errors =================================
#--- Durbin-Watson Test for Autocorrelated Errors --------
# Computes residual autocorrelations and generalized Durbin-Watson statistics and their bootstrapped p-values. 
# dwt is an abbreviation for durbinWatsonTest
ace1 <- durbinWatsonTest(fit, max.lag=10)
print(ace1)

#=== Global test of model assumptions ============================
gvmodel <- gvlma(fit) 
tma     <- summary(gvmodel)
print(tma)


#=== Other plots ===================
# cio2   <- plot(fit, which=1, cook.levels=cutoff) # Residuals vs Fitted
# cio2   <- plot(fit, which=2, cook.levels=cutoff) # Normal Q-Q (Theoretical quantiles vs. Standardized residuals)
# cio2   <- plot(fit, which=3, cook.levels=cutoff) # Scale-Location (Fitted vs sqrt of standardized residuals)
# cio2   <- plot(fit, which=5, cook.levels=cutoff) # Residuals vs Leverage (Leverage vs Standardized residuals)
# cio2   <- plot(fit, which=6, cook.levels=cutoff) # Cook'S distance vs Leverage
