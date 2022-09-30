#------------------------------#
#--- Deming Regression in R ---#
#------------------------------#


# Let's make up some data with normal errors
x = seq(10,20,0.25)
y_ideal = x
N = length(x)
sd = 2
y = rnorm(N,y_ideal,sd)  # rnorm(n,mean,sd)

# perform the linear regression
model.ygivenx = lm(y~x)
summary(model.ygivenx)

model.xgiveny = lm(x~y)
summary(model.xgiveny)

GM.slope = sqrt(model.ygivenx$coefficients[2]/model.xgiveny$coefficients[2])
GM.slope

# Now let's assume that each x and sigma=1 error and that each y has sigma=sqrt(2) error (this matches our simulated data)

lambda = var(y)/var(x)  # use this lambda to make Deming equal to GM
lambda = 1  # use this lambda to make Deming equal to OR

lambda = 4  # for our example here

#install.packages("mcr")
library(mcr)  # method comparison regression
dem.reg <- mcreg(x,y, method.reg = "Deming", error.ratio = 1/lambda, method.ci = "analytical")
printSummary(dem.reg)
#method.ci = "bootstrap", "jackknife", "analytical", or "nestedbootstrap"
str(dem.reg)  # to get everything out of the regression result

getResiduals(dem.reg) #returns x, y, and optimized residuals

# Here is our analytical formula for SE(slope):
SE_slope = dem.reg@para[2]/sqrt(N)*sqrt(1/(model.ygivenx$coefficients[2]*model.xgiveny$coefficients[2])-1)
SE_slope

plot(x,y)
abline(dem.reg@para[1:2], col = "blue")
abline(model.ygivenx$coefficients[1:2], col = "red")
legend("topleft", c("OLS","Deming"), lty=c(1,1), lwd=c(2,2), col=c("red","blue"), bty="n") 
