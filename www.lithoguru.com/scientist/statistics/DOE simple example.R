#--------------------------------------#
#--- Simple DOE Linear Example in R ---#
#--------------------------------------#


# two example designs for a simple lienar regression

# evenly spaced x values
x1 = seq(5,19.5,0.5)
# dumbbell design
x2 = c(rep(5,15),rep(19.5,15))  
# quadratic design
x3 = c(rep(5,10),rep(12.25,10),rep(19.5,10))  

# now create y values that are a linear function of the x plus a normal error term
n = length(x1)
error = rnorm(n, mean = 0, sd = 1)
y1 = 3 + x1 + error
y2 = 3 + x2 + error
y3 = 3 + x3 + error

# Perform OLS on each data set
model1 = lm(y1 ~ x1)
model2 = lm(y2 ~ x2)
model3 = lm(y3 ~ x3)

SE_slope1 = summary(model1)$coefficients[ 2, 2]  # standard error of the slope
SE_slope2 = summary(model2)$coefficients[ 2, 2]
SE_slope3 = summary(model3)$coefficients[ 2, 2]

# Which design produces the lower standard error of the slope?

cat("    Dumbbell Design SE(b1) = ", format(SE_slope2, digits = 4), "\n", "  Quadratic Design SE(b1) = ", format(SE_slope3, digits = 4), "\n","Even-Spaced Design SE(b1) = ", format(SE_slope1, digits = 4) )
cat("      Dumbbell Design SE(resids) = ", format(summary(model2)$sigma, digits = 4), "\n", "    Quadratic Design SE(resids) = ", format(summary(model3)$sigma, digits = 4), "\n", "Evenly Spaced Design SE(resids) = ", format(summary(model1)$sigma, digits = 4))
