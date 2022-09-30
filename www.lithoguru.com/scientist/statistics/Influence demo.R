#-----------------------------------#
#--- Leverage and Influence in R ---#
#-----------------------------------#


#Importing data: use .csv format if at all possible
Flow <- read.csv('Flow Rate Calibration.csv')

#Step 1:  plot the data
plot(Flow$Flow.Difference ~ Flow$Temperature, data = Flow)

#Step 2:  OLS using the lm function
model = lm(Flow$Flow.Difference ~ Flow$Temperature, data = Flow)
summary(model)

library(MASS)  #for the studres function
esr = studres(model) # externally studentized residuals (from MASS library)

#Step 3: plot the externally studentized residuals (esr)
plot(esr ~ fitted(model), xlab = "Fitted Values", ylab = "esr")

#plot the absolute value of the externally studentized residuals (esr)
plot(abs(esr) ~ fitted(model), xlab = "Fitted Values", ylab = "|esr|")


hatvalues(model)  # this produces the diagonal of the hat matrix (the leverage)

# Let's create a Williams Graph
norm_leverage = (length(esr)/2)*hatvalues(model)
plot(abs(esr) ~ norm_leverage, xlab = "leverage*n/p", ylab = "|esr|", main ="Williams Graph")


#plotting options specifically for lm objects
plot(model, which = 4)
#1 = residuals versus fitted
#2 = QQ plot
#3 = Scale-Location
#4 = Cook's Distance
#5 = Williams-like Graph

D = cooks.distance(model) #this provides the cook's Distance for each data points

#Some other options for a Cook's Distance graph
plot(D ~ Flow$Temperature, xlab = "Temperature (K)", ylab = "Cook's D")
plot(D ~ fitted(model), xlab = "% Flow Difference", ylab = "Cook's D")
max(D)

# we can also calculate DFFITS and DFBETA
dffits(model)

biggest = max(abs(dffits(model)))
biggest

dfbeta(model)
biggest = max(abs(dfbeta(model)))
biggest
