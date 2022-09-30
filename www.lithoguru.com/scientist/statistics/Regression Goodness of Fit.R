#---------------------------------------#
#--- Regression Goodness of Fit in R ---#
#---------------------------------------#


#When importing data, it is best to use .csv format
#Here, I typed in the y-values directly using the Concatonate function c
y = c(4.26, 5.68, 7.24, 4.82, 6.95, 8.81, 8.04, 8.33, 10.84, 7.58, 9.96)
#the x-values are evenly spaced from 4 to 14
x = 4:14 #the column gives an array of all integers from 4 to 14
x = seq(4,14,1)  #you can use the sequence function to do the same thing

#Step 1:  plot the data
plot(y ~ x)

#Step 2:  OLS using the lm function
model = lm(y ~ x)
summary(model)
?summary.lm
myR2 = summary(model)$r.squared

# Add the regression line to the data plot
lines(x, predict(model), col = "red")

#-----------------------------------------------------------------------
# Perform an overall F-test for the regression 
# (automiatically done in summary.lm)

myF = summary(model)$fstatistic[1]
# fstatistic[2] = numerator degress of freedom
# fstatistic[3] = denominator degress of freedom

# for a two-parameter model, F = t(slope)^2
myTsquare = (summary(model)$coefficients[2,3])^2
# coefficients = p x 4 matrix with columns for the estimated coefficient, its standard error, t-statistic and corresponding (two-sided) p-value

#-----------------------------------------------------------------------
#Perform a chi-square test for the model

# standard error of the residuals 
mySEres = summary(model)$sigma
?summary.lm

myDF = summary(model)$df[2]  # degrees of freedom of the residuals
# df[1] = number of non-aliased parameters
# df[2] = residual degress of freedom, n - p
# df[3] = number of parameters, p

# suppose we know the measurement error for y:
sigma_ME = 0.8

# test the chi-square
myChisquare = myDF*(mySEres^2/sigma_ME^2)

pvalue = 1-pchisq(myChisquare,df = myDF)  # right tailed probability
