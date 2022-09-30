#-----------------------------#
#--- Ridge Regression in R ---#
#-----------------------------#


# Let's create a data set with large multicollinearity
x1 <- rnorm(20)
x2 <- rnorm(20, mean=x1, sd=0.01)
cor(x1,x2)
y <- rnorm(20, mean=3+x1+x2, sd = 0.2)

# Try OLS
lm(y~x1+x2)$coef

#Now try Ridge Regression
library(MASS)
lm_seq = seq(0,1,.001)
fit = lm.ridge(y~x1+x2,lambda=lm_seq)
select(fit)

lm.ridge(y~x1+x2,lambda=0.1)

# Let's try it on a real data set
bodyfat1 <- read.csv('bodyfat-reduced.csv')

#standardize our data before doing a ridge regression
bodyfat = data.frame(scale(bodyfat1))
plot(bodyfat)

#calculate the correlation matrix
myCorr = cor(bodyfat)
myCorr

# Condition Number: Ratio of max to min Eigen values of the correlation matrix
max(eigen(cor(bodyfat))$values)/min(eigen(cor(bodyfat))$values)
kappa(cor(bodyfat), exact = TRUE)


model = lm(BodyFat~., data = bodyfat)
summary(model)

# variance inflation factors (Are any > 5 or 10?)
library(car)
vif(model) 
mean(vif(model))  #Is the mean VIF much bigger than 1?



# Now try a ridge regression
library(MASS)

lm_seq = seq(0,10,0.01)
fit = lm.ridge(BodyFat ~ ., data = bodyfat, lambda = lm_seq)
# plot the trace of each coefficient over a range of ridge coefficients (lambda)
plot(fit)
# Find the value of lambda that is "best
select(fit)

# GCV = generalized crosss validation
plot(lm_seq, fit$GCV, main="GCV of Ridge Regression", type="l", xlab=expression(lambda), ylab="GCV")

lm.ridge(BodyFat ~ ., data = bodyfat, lambda = 0.8)
lm.ridge(BodyFat ~ ., data = bodyfat, lambda = 0.)
