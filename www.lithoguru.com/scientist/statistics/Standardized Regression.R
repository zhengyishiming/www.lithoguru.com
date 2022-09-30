#-----------------------------------#
#--- Multiple LS Regression in R ---#
#-----------------------------------#


#When importing data, it is best to use .csv format
bodyfat <- read.csv('bodyfat-reduced.csv')

plot(bodyfat)

#calcualte the correlation matrix
myCorr = cor(bodyfat)
myCorr

# Standardize each variable (substract mean, divide by sd)
bodyfat2 = data.frame(scale(bodyfat))
plot(bodyfat2)

#calcualte the correlation matrix
myCorr2 = cor(bodyfat2)
myCorr2

#compare models with and without standardization
model1 = lm(BodyFat~Abdomen*Chest, data = bodyfat)
summary(model1)
confint(model1, level=0.95)

model2 = lm(BodyFat~Abdomen*Chest, data = bodyfat2)
summary(model2)
confint(model2, level=0.95)
