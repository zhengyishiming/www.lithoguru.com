#-----------------------------------#
#--- All Subset Regression in R ---#
#-----------------------------------#


# When importing data, it is best to use .csv format
bodyfat <- read.csv('bodyfat.csv')

dim(bodyfat)

# Regression subset selection including exhaustive search, use the "leaps" package
# install.packages("leaps")
library(leaps)
library(car)

# nbest	= number of best subsets of each size to keep in the results (default=1)
# nvmax = maximum size of subsets to examine
# Period notation regresses BodyFat against all the other variables in data set
leaps<-regsubsets(BodyFat~., data=bodyfat, nbest = 1, method = "forward", nvmax = 11)

# plot statistic by subset size (rsq, cp, adjr2, bic, rss)
subsets(leaps, statistic="bic")

leaps<-regsubsets(BodyFat~., data=bodyfat, nbest = 1, method = "backward", nvmax = 11)
subsets(leaps, statistic="bic")

leaps<-regsubsets(BodyFat~., data=bodyfat, nbest = 1, method = "exhaustive", nvmax = 11)
subsets(leaps, statistic="bic")

leaps<-regsubsets(BodyFat~.^2, data=bodyfat, nbest = 1, method = "forward", nvmax = 11)
subsets(leaps, statistic="bic", xlim = c(0,12))

leaps<-regsubsets(BodyFat~.^2, data=bodyfat, nbest = 1, method = "backward", nvmax = 11)
subsets(leaps, statistic="bic", xlim = c(0,12))

leaps<-regsubsets(BodyFat~.^2, data=bodyfat, nbest = 1, method = "exhaustive", nvmax = 6)
subsets(leaps, statistic="bic", xlim = c(0,12))


# stepwize search for best regression model
library(MASS)
fit <- lm(BodyFat~., data=bodyfat)
step <- stepAIC(fit, direction="both")
step$anova # display results
