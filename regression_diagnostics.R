# Taken from: http://www.statmethods.net/stats/rdiagnostics.html
library(car)
library(gvlma)
library(MASS)

data <- v_dataShortProc$data

fit <- lm(log_E_CC ~  log_GDPpc + log_Manu + log_Manu_Ex + log_P + log_P_Ndep + log_URB, data=data)

# Assessing Outliers
outlierTest(fit) # Bonferonni p-value for most extreme obs
qqPlot(fit, main="QQ Plot") #qq plot for studentized resid 
leveragePlots(fit) # leverage plots

# Influential Observations
# added variable plots 
av.Plots(fit)
# Cook's D plot
# identify D values > 4/(n-k-1) 
cutoff <- 4/((nrow(data)-length(fit$coefficients)-2)) 
plot(fit, which=4, cook.levels=cutoff)
# Influence Plot 
influencePlot(fit,	id.method="identify", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )

# Normality of Residuals
# qq plot for studentized resid
qqPlot(fit, main="QQ Plot")
# distribution of studentized residuals
sresid <- studres(fit) 
hist(sresid, freq=FALSE, 
     main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40) 
yfit<-dnorm(xfit) 
lines(xfit, yfit)

# Evaluate homoscedasticity (Non-constant error variance)
# non-constant error variance test
ncvTest(fit)
# plot studentized residuals vs. fitted values 
spreadLevelPlot(fit)

# Evaluate Collinearity
vif(fit) # variance inflation factors 
sqrt(vif(fit)) > 2 # problem?

# Evaluate Nonlinearity
# component + residual plot 
crPlots(fit)
# Ceres plots 
ceresPlots(fit)

# Test for Autocorrelated Errors
durbinWatsonTest(fit)

# Global test of model assumptions
gvmodel <- gvlma(fit) 
summary(gvmodel)