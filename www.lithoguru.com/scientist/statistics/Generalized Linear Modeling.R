#----------------------------------------#
#--- Generalized Linear Modeling in R ---#
#----------------------------------------#


#When importing data, it is best to use .csv format
#Here, I typed in the y-values directly using the Concatonate function c
y = c(4.26, 5.68, 7.24, 4.82, 6.95, 8.81, 8.04, 8.33, 10.84, 7.58, 9.96)
#the x-values are evenly spaced from 4 to 14
x = 4:14 #the column gives an array of all integers from 4 to 14
x = seq(4,14,1)  #you can use the sequence function to do the same thing

#Step 1:  plot the data
plot(y ~ x)

# OLS using the lm function
model = lm(y ~ x)
summary(model)

# OLS using the generalized linear modeling routine glm:
model.glm = glm(y ~ x, family=gaussian(link = "identity"))
summary(model.glm)

# Family	          Default Link Function
# binomial	        (link = "logit")
# gaussian	        (link = "identity")
# Gamma	            (link = "inverse")
# inverse.gaussian	(link = "1/mu^2")
# poisson	          (link = "log")
# quasi	            (link = "identity", variance = "constant")
# quasibinomial	    (link = "logit")
# quasipoisson	    (link = "log")



# Here is an interesting example of logistic regression:
# Who survived the sinking of the Titanic?
# https://www.r-bloggers.com/how-to-perform-a-logistic-regression-in-r/
# install.packages("titanic")
library(titanic)

data.raw = titanic_train

# This data has blanks.  Let's convert the blanks to "NA"
data.raw[data.raw==""]  <- NA 

# Check for missing values using the sapply() function
sapply(data.raw, function(x) sum(is.na(x)))
length(data.raw$Pclass)  # Number of data points
levels(factor(data.raw$Embarked))

# Let's drop the columns that we know we are not going to use:
data <- subset(data.raw,select=c(2,3,5,6,7,8,10,12))


# Now let's perform a logistic regression
model <- glm(Survived ~ Sex,family=binomial(link='logit'),data=data)
summary(model)
confint(model)  #confidence intervals for the coefficients

exp(model$coef)
