#----------------------------------------#
#--- Detecting Multicollinearity in R ---#
#----------------------------------------#


#When importing data, it is best to use .csv format
bodyfat <- read.csv('bodyfat-reduced.csv')

plot(bodyfat)

#calculate the correlation matrix
myCorr = cor(bodyfat)
myCorr

# Eigensystem Analysis
eigen(cor(bodyfat))$values

# Condition Number: Ratio of max to min Eigen values of the correlation matrix
max(eigen(cor(bodyfat))$values)/min(eigen(cor(bodyfat))$values)
kappa(cor(bodyfat), exact = TRUE)


model = lm(BodyFat~., data = bodyfat)

# variance inflation factors (Are any > 5 or 10?)
library(car)
vif(model) 
mean(vif(model))  #Is the mean VIF much bigger than 1?
