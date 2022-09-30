#--------------------------------------#
#--- Response Surface Modeling in R ---#
#--------------------------------------#

#First, install and load the "rsm" package

# install.packages("rsm")
library(rsm)

# Example generating a Box-Behnken design with three factors and two center points (no)
bbd(3, n0 = 2, coding = list(x1 ~ (Force - 20)/3, x2 ~ (Rate - 50)/10, x3 ~ Polish - 4))


# Example data set
data = ChemReact
plot(data)


# The data set was collected in two blocks.
# Block1 is a 2-level, two-factor factorial design with three repeated center points.
# Block 2 is the Central Composite Design (circomscribed) with 3 center points.
# The variables are Time = 85 +/- 5 and Temp = 175 +/- 5, 
# Thus, the coded variables are  x1 = (Time-85)/5 and x2 = (Temp-175)/5
CR <- coded.data(ChemReact, x1 ~ (Time - 85)/5, x2 ~ (Temp - 175)/5)
CR[1:7,]

# Note: If the data are already coded, use as.coded.data() to convert to the proper coded data object

# Let's work as though the first block (full factorial) has been finished, 
# and we'll fit a linear model, first order (FO), to it (Yield is the response)
CR.rsm1 <- rsm(Yield ~ FO(x1, x2), data = CR, subset = (Block == "B1"))
summary(CR.rsm1)

#The fit is not very good.  Let's include the interaction term (TWI) and update the model, or start over with a new model (these two lines do the same thing)
CR.rsm1.5 <- update(CR.rsm1, . ~ . + TWI(x1, x2))
CR.rsm1.5 <- rsm(Yield ~ FO(x1, x2)+TWI(x1, x2), data = CR, subset = (Block == "B1"))
summary(CR.rsm1.5)
#This is no better!  The reason is the strong quadratic response, with the peak near the center.

# Now let's assume the second block has been collected.  We use the SO (second order) function, which includes FO and TWI
CR.rsm2 <- rsm(Yield ~ Block + SO(x1, x2), data = CR)
summary(CR.rsm2)

# The secondary point is a maximum (both eigenvalues are negative) and within the experimental design range (no extrapolation)

# Also note that the block is significant, meaning that the processes shifted between the first set of data and the second.  This is not good.  The coefficient is -4.5, meaning the yield shifted down by 4.5% between the two blocks - a more significant effect than either temperatue or time!  This is most easily seen by looking at the repeat center points.

# We can plot the fitted response as a contour plot.
contour(CR.rsm2, ~ x1 + x2, at = summary(CR.rsm2)$canonical$xs)
