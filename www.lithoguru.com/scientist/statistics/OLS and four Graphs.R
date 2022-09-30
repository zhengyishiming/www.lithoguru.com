#-----------------------------------------------#
#--- Graphing Linear Regression Results in R ---#
#-----------------------------------------------#

# Here is a very basic regression example, generating four useful plots

# Let's make up some data with normal errors
x = seq(10,40,0.5)
y_ideal = x^2
N = length(x)
sd = 20
y = rnorm(N,y_ideal,sd)  # rnorm(n,mean,sd)

# perform the linear regression
model = lm(y~I(x^2))  # I(x^2) means include the x^2 term (only) in the model
summary(model)

# Now generate a series of four plots about the data and model
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






# More on plotting

plot(y ~ x, cex = 1.3)  # cex = 1.3 makes the data points 1.3X bigger

# bonus topic: LOWESS = Locally Weighted Scatterplot Smoothing
# this creates a nonparametric "smooth" line through the data
# it is usefull for seeing trends in the data (before modeling)
lines(lowess(x,y), col="blue")

# here is an enhanced plotting function, where the LOWESS line and/or
# a straight regression line can be automatically plotted as well
library(car)
scatterplot(y ~ x, smoother="FALSE", reg.line="FALSE")

# Other plotting systems in R are lattice and ggplot
