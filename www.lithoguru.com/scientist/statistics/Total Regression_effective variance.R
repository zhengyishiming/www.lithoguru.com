#-----------------------------#
#--- Total Regression in R ---#
#-----------------------------#

fourgraphsummary <- function(model){
  
  # This routine produces a four graph summary of a regression model
  
  par(mfrow=c(2,2))
  
  # plot the data and the best fit model
  plot(y ~ x, cex = 1.3, main = "Data and Model")  
  #now add the model line to the plot
  lines(x, predict(model), col = "red")
  
  # plot the residuals
  library(MASS)  #for the studres function
  esr = studres(model) # externally studentized residuals (from MASS library)
  plot(esr~predict(model), main = "Residuals")
  
  # Normal Probability Plot
  qqnorm(esr)
  qqline(esr, col = "red")
  
  # Let's create a Williams Graph
  norm_leverage = (length(esr)/2)*hatvalues(model)
  plot(abs(esr) ~ norm_leverage, xlab = "leverage*n/p", ylab = "|esr|", main ="Williams Graph")
  
  # add the critical lines to the Williams Graph
  N = length(esr)
  p = length(model$coefficients)
  alpha = 0.01
  tinv = qt(alpha/2/N,N-p-1)
  Grubbs_crit = (N-1)/sqrt(N)*sqrt(tinv^2/(N-3+tinv^2))
  abline(h=Grubbs_crit, col="red") # draw a red horizontal line at Gcrit
  abline(v=2, col="red") # draw a red vertical line at 2
  
  return
}

# Let's make up some data with normal errors
x = seq(10,40,0.5)
y_ideal = x^2
N = length(x)
sd = sqrt(100+9*x^2)
y = rnorm(N,y_ideal,sd)  # rnorm(n,mean,sd)

# perform the linear regression
model = lm(y~I(x^2))  # I(x^2) means include the x^2 term (only) in the model
summary(model)
fourgraphsummary(model)

# Now let's assume that each x has sigma=1.5 error and that each y has sigma=10 error (this matches our simulated data)

# We'll start with the effective variance method
# The slope will be 2*b1*x since the model term is x^2
b1 = model$coefficients[2]
slope = 2*b1*x
sdx = slope*1.5
sdy = 10
effectivevariance = sdy^2 + sdx^2
w = 1/effectivevariance

#Improvement:  estimate the xi using the previous model fit
#x = x + (resid(model)/slope)*(sdx^2/effectivevariance)

model2 = lm(y~I(x^2), weights = w)
summary(model2)
fourgraphsummary(model2)

# this third iteration is not be necessary, except to prove that our slope estimate has stablized!
b1 = model2$coefficients[2]
slope = 2*b1*x
sdx = slope*1.5
sdy = 10
effectivevariance = sdy^2 + sdx^2
w = 1/effectivevariance
#x = x + (resid(model2)/slope)*(sdx^2/effectivevariance)

model3 = lm(y~I(x^2), weights = w)  # I(x^2) means include the x^2 term (only) in the model
summary(model3)
fourgraphsummary(model3)

coefficients(model)
coefficients(model2)
coefficients(model3)
