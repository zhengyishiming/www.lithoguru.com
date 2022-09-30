#----------------------------------------#
#--- Weighted Regression Results in R ---#
#----------------------------------------#

# Let's make up some data and weights
x = 1:5
y = c(1.1,2.5,3.4,3.8,7)

sd = c(0.3, 0.2, 0.2, 0.1, 0.5)
w = 1/sd^2

# perform the linear regression
model0 = lm(y~x)
summary(model0)

#now perform the weighted regression
model = lm(y~x, weights = w)
summary(model)

# here are all the forms of residuals
raw = resid(model)  # the raw residuals
isr = stdres(model)  # internally studentized residuals
library(MASS)  # needed for the studres function
esr = studres(model)  # externally studentized residuals
wres = weighted.residuals(model)  # weighted raw residuals
# Note that isr and esr are calculated from the weighted residuals

# plot the data and the best fit models
plot(y ~ x, cex = 1.3, main = "Data and Model")  
#now add the model line to the plot
lines(x, predict(model), col = "red", lwd=2)
lines(x, predict(model0), col = "blue", lwd=2)

legend("topleft", c("Weighted","Unweighted"), lty=c(1,1), lwd=c(2,2), col=c("red","blue"), bty="n") 

#manual calculation of the internally studentized residuals
res_SE = summary(model)$sigma
h = hatvalues(model)
isr_manual = wres/res_SE/sqrt(1-h)  # isr uses the weighted residuals
isr = stdres(model)  # This should give the exact same results


