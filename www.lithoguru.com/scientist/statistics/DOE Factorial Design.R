#-----------------------------#
#--- Factorial Design in R ---#
#-----------------------------#

#First, some tools for creating designs

# install.packages("AlgDesign")
library(AlgDesign)

# As an example, we'll create full factorial design with three factors (F1, F2, F3) and
# with 3, 2, and 3 levels, respectively
gen.factorial(levels = c(3,2,3), nVars = 3, center=TRUE, varNames=c("F1", "F2", "F3"))
# When center = TRUE, the design is symmetric and the three levels will be signified by -1, 0, and 1

# The common 2^k design (two levels for each factor):
dat = gen.factorial(levels = 2, nVars = 3, varNames=c("F1", "F2", "F3"))
dat
# the output is a data frame containing the factorial design








# Factorial Design example from Box, Hunter, and Hunter, Statistics for Experimenters, John Wiley & Sons (1978).
yield <- read.csv('FactorialDesign.csv')
# Note that this is a two-level, three factor, full factorial design with Yield as the response (no replicates)

# remove the "Run" column
yield <- yield[-1]

# There are data for two catalysts (0, 1); let's convert it into a factor
yield$Catalyst = factor(yield$Catalyst)

# A few plots can help us see the data
plot(yield)

par(mfrow=c(2,2))
interaction.plot(yield$Temp,yield$Conc,yield$Yield)
interaction.plot(yield$Temp,yield$Catalyst,yield$Yield)
interaction.plot(yield$Conc,yield$Catalyst,yield$Yield) 
par(mfrow=c(1,1))

# install.packages("lattice")
library(lattice)

xyplot(Yield ~ Temp | Catalyst, data = yield,
       panel = function(x, y, ...)
       {
         panel.xyplot(x, y, ...)
         panel.lmline(x, y, ...)
       }
)
xyplot(Yield ~ Conc | Catalyst, data = yield,
       panel = function(x, y, ...)
       {
         panel.xyplot(x, y, ...)
         panel.lmline(x, y, ...)
       }
)

par(mfrow=c(2,2))
boxplot(Yield~Temp, data=yield, main="Yield by Temperature",
        xlab="Temperature (C)",ylab="Percent Yield")

boxplot(Yield~Conc, data=yield, main="Yield by Concentration",
        xlab="Concentraion (wt%)",ylab="Percent Yield")

boxplot(Yield~Catalyst, data=yield, main="Yield by Catalyst",
        xlab="Catalyst Type",ylab="Percent Yield")
par(mfrow=c(1,1))

# Alternately, combine boxpolts into one graph
boxplot(Yield ~ Temp*Conc, data=yield, ylab="% Yield", xlab="Temp.Conc")
boxplot(Yield ~ Temp*Catalyst, data=yield, ylab="% Yield", xlab="Temp.Catalyst")

# If you find the printing of significance stars annoying, you can turn them off (for model and ANOVA summary outputs)
options(show.signif.stars=F)

# let's model the main effects
model1 = lm(Yield ~ ., data = yield)
summary(model1)

# Now we'll add two-factor interactions
model2 = lm(Yield ~ .^2, data = yield)
summary(model2)

#compare the two models
anova(model1, model2)

# Finally, the full model including three-factor interactions
model3 = lm(Yield ~ .^3, data = yield)
summary(model3)
# Note that since we have no replicates, we have no way of assessing the significance of any of the coefficients of this full model with all interactions

# also note that we could model this with some, but not all of the interactions present
model4 = lm(Yield ~ Conc + Temp*Catalyst, data = yield)
summary(model4)
# Note that Y ~ X1 * X2 is the same as Y ~ X1 + X2 + X1 : X2
# and ":" means 'interaction between'

anova(model4,model2)






# Another way to model the response as a function of the factors is with an anova table.  This requires all variables to be factors.

yield.aov = yield
yield.aov$Conc = factor(yield.aov$Conc)
yield.aov$Temp = factor(yield.aov$Temp)

aov1.out = aov(Yield ~ ., data=yield.aov)
summary(aov1.out)

aov2.out = aov(Yield ~ .^2, data=yield.aov)
summary(aov2.out)

# This provides all of the means commonly calculated in ANOVA analysis
model.tables(aov2.out, type="means", se=T)

aov3.out = aov(Yield ~ .^3, data=yield.aov)
summary(aov3.out)

# Based on these results, I think a Conc + Temp*Catalyst model may be the best
aov4.out = aov(Yield ~ Conc + Temp*Catalyst, data=yield.aov)
summary(aov4.out)

# You can plot the ANOVA results in a couple of ways:
plot(aov4.out, 1)           # residuals versus fitted values
plot(aov4.out, 2)           # QQ plot
