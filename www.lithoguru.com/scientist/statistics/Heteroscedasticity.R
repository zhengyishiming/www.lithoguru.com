#-----------------------------------------#
#--- Testing for Homoscedasticity in R ---#
#-----------------------------------------#


#Importing data: use .csv format if at all possible
Flow_unsorted <- read.csv('Flow Rate Calibration.csv')
str(Flow_unsorted)

# we are going to need the residuals sorted by increasing temperature below
# for the homoscedasticity tests
Flow = Flow_unsorted[order(Flow_unsorted$Temperature),]
str(Flow)

#Step 1:  plot the data
plot(Flow$Flow.Difference ~ Flow$Temperature, data = Flow)

#Step 2:  OLS using the lm function
model = lm(Flow$Flow.Difference ~ Flow$Temperature, data = Flow)
summary(model)


#Perform the Bartlett test (assumes a normal distribution of residuals)
library(MASS)
esr <- studres(model)  #extract the externally studentized residuals

# split the data in half
N = length(esr)
group = rep(2,N)
group[1:N/2] = 1

bartlett.test(esr, as.factor(group))
# If the p-value is less than alpha, we reject the null hypothesis that
# the variances of each group are equal.

#install.packages("lawstat")
library(lawstat)
levene.test(esr, as.factor(group))
# Note that this test is formualted using an F statistic, rather than the
# t-statistic that was discussed in class.  They work out the same, giving
# the same p-value.


# Evaluate homoscedasticity using the Breusch-Pagan test
# This test creates a stright-line fit of esr^2 to either the
# fitted values or the explanatory variables, then checks the 
# magnitude of R^2 for the fit

#install.packages("car")
#car contains Functions and Datasets to Accompany J. Fox and S. Weisberg, An R Companion to Applied Regression, Second Edition, Sage, 2011.
library(car)
ncvTest(model)  #defaults to testing against fitted values

#Here is an alternate package with regression diagnostics:  lmtest
#install.packages("lmtest")
library(lmtest)
bptest(model)  #defaults to to testing against explanatory variables


#small p-value indicates heteroscedasticity
