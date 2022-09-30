#-----------------------------------#
#--- Analysis of Covariance in R ---#
#-----------------------------------#

# Example from: 
# https://www.r-bloggers.com/analysis-of-covariance-%E2%80%93-extending-simple-linear-regression/

# We'll grab the "Orange" data frame that is in R
orange.df = Orange

# There are data for five trees, labled 1 - 5
# We need to convert the Tree identifier into a factor
orange.df$Tree = factor(as.numeric(orange.df$Tree))
is.factor(orange.df$Tree)

# First, we plot the data
plot(orange.df$circumference~orange.df$age)

#let's model tree circumfrance as linear with age
orange.mod1 = lm(circumference ~ age, data = orange.df)
summary(orange.mod1)

# Second model:  a  separate intercept for each of the five trees. 
orange.mod2 = lm(circumference ~ age + Tree, data = orange.df)
summary(orange.mod2)

anova(orange.mod1, orange.mod2)
# There is very strong evidence of a difference in starting circumference between the trees.
# We can extended this model further by allowing the rate of increase in circumference to vary between the five trees.  Assume that tree 1 is the baseline.  
orange.mod3 = lm(circumference ~ age + Tree + age:Tree, data = orange.df)
summary(orange.mod3)

# there is strong evidence of a difference in the rate of change in circumference for the five trees. 
# The panel.xyplot and panel.lmline functions are part of the lattice package 
# install.packages("lattice")
library(lattice)

xyplot(circumference ~ age | Tree, data = orange.df,
  panel = function(x, y, ...)
  {
    panel.xyplot(x, y, ...)
    panel.lmline(x, y, ...)
  }
)
# plot residuals against fitted values, divided by tree, to investigate the model assumptions:
xyplot(resid(orange.mod3) ~ fitted(orange.mod3) | orange.df$Tree,
  xlab = "Fitted Values",
  ylab = "Residuals",
  main = "Residual Diagnostic Plot",
  panel = function(x, y, ...)
  {
    panel.grid(h = -1, v = -1)
    panel.abline(h = 0)
    panel.xyplot(x, y, ...)
  }
)

anova(orange.mod1, orange.mod3)

