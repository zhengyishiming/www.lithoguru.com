#------------------------------#
#--- Robust Regression in R ---#
#------------------------"------#


# The robustbase package contains a demonstration dataset called hbk
# install.packages("robustbase")
library(robustbase)
data(hbk)

plot(hbk)

# using OLS:
model.lm = lm(Y~., data = hbk)
summary(model.lm)
confint(model.lm, level=0.95)

hbk.x <- data.matrix(hbk[,1:3])

library(MASS)

cat("Measurements of location:")
cat("mean(x1) = ", mean(hbk[,1]))
cat("median(x1) = ", median(hbk[,1]))
cat("20% trimmed mean(x1) = ", mean(hbk[,1], trim = 0.2))   #20% off both ends
cat("covMcd location(x1) = ", covMcd(hbk.x)$center[1])
cat("cov.rob location(x1) = ", cov.rob(hbk.x)$center[1])

cat("Measurements of scale:")
cat("StdDev(x1) = ", sd(hbk[,1]))
cat("MAD(x1) = ", mad(hbk[,1]))   # The default constant = 1.4826
cat("0.7413*IQR(x1) = ", 0.7413*IQR(hbk[,1]))
cat("covMcd scale(x1) = ", sqrt(covMcd(hbk.x)$cov[1,1]))
cat("cov.rob scale(x1) = ", sqrt(cov.rob(hbk.x)$cov[1,1]))

bodyfat <- read.csv('bodyfat.csv')

model.lm = lm(BodyFat~Abdomen, data = bodyfat)
summary(model.lm)
model.robust1 = rlm(BodyFat~Abdomen, data = bodyfat)
summary(model.robust1)
bodyfat2 <- read.csv('bodyfat-reduced.csv')
model.lm2 = lm(BodyFat~Abdomen, data = bodyfat2)
summary(model.lm2)


# robust regression
library(MASS)
model.robust1 = rlm(Y~., data = hbk)
summary(model.robust1)
# Fitting is done by iterated re-weighted least squares
# method = "M" gives M-estimation (default), method = "MM" gives M-estimation with Tukey's biweight initialized by a specific S-estimator.
# psi = psi.huber (default), psi.hampel, or psi.bisquare.
# scale.est	= method of scale estimation; default = re-scaled MAD of the residuals


library(robustbase)
model.robust2 = lmrob(Y~., data = hbk, method = "MM")
summary(model.robust2)
# default method = "MM" gives M-estimation with Tukey's biweight initialized by a specific S-estimator.


# install.packages("robust")
library(robust)
model.robust3 = lmRob(Y~., data = hbk)
summary(model.robust3)
