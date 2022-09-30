#----------------------------------#
#--- Durbin-Watson Testing in R ---#
#----------------------------------#


#Importing data: use .csv format if at all possible
Flow <- read.csv('Flow Rate Calibration.csv')

#Step 1:  plot the data
plot(Flow$Flow.Difference ~ Flow$Temperature, data = Flow)

#Step 2:  OLS using the lm function
model = lm(Flow$Flow.Difference ~ Flow$Temperature, data = Flow)
summary(model)

# Add the regression line to the data plot
lines(Flow$Temperature, predict(model), col = "red")

# Create a lag plot with lag = 1
lag.plot(model$residuals, lags = 1, do.lines = F, labels = F)
model$residuals
#Durbin-Watson Test
#install.packages("lmtest")
library(lmtest)
dwtest(model)

# Plot the autocorelation coefficeints as a function of lag, r(k)
# by default, the correlation coefficient is provided
rho = acf(model$residuals)
r = rho[1]$acf[1,,]
#Note: the method used to estimate rho is not the same as the correlation coefficient

#now transform the data using the AR(1) model
n = length(model$residuals)
yprime = rep(0,n-1)
xprime = rep(0,n-1)
for (i in 1:n-1){
  yprime[i] = Flow$Flow.Difference[i+1] - r*Flow$Flow.Difference[i]
  xprime[i] = Flow$Temperature[i+1] - r*Flow$Temperature[i]
}

plot(yprime ~ xprime)

#Step 2:  OLS using the lm function
model2 = lm(yprime ~ xprime)
summary(model2)

# Add the regression line to the data plot
lines(xprime, predict(model2), col = "red")

dwtest(model2)
