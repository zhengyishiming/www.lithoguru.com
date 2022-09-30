#-----------------------------------#
#--- All Subset Regression in R ---#
#-----------------------------------#


# When importing data, it is best to use .csv format
bodyfat <- read.csv('bodyfat-reduced.csv')

plot(bodyfat)

# OLS using the lm function for the full model
model2 = lm(BodyFat~Abdomen+Thigh, data = bodyfat)
summary(model2)

# OLS using the lm function for the subset model
model1 = lm(BodyFat~Abdomen, data = bodyfat)
summary(model1)

# Perform a partial F-test using the anova 
# Null hypothesis:  the extra parameters in the full model have coefficients=0
anova(model1, model2)

# Perform a likelihood ratio test
# Null hypothesis:  the extra parameters in the full model have coefficients=0
library(lmtest)
lrtest(model1,model2)

# Regression subset selection including exhaustive search, use the "leaps" package
# install.packages("leaps")
library(leaps)

# nbest	= number of best subsets of each size to keep in the results (default=1)
# Period notation regresses BodyFat against all the other variables in data set
leaps<-regsubsets(BodyFat~., data=bodyfat, nbest = 5, method = "exhaustive")

# this period notation regresses BodyFat against all the other variables in the data set plus all interactions
leaps<-regsubsets(BodyFat~.^2, data=bodyfat, nbest = 10, method = "exhaustive")
# view results 
summary(leaps)

# plot statistic by subset size (rsq, cp, adjr2, bic, rss)
library(car)
subsets(leaps, statistic="bic")






# stepwizesearch for best regression model
library(MASS)
fit <- lm(BodyFat~., data=bodyfat)
step <- stepAIC(fit, direction="both")
step$anova # display results
