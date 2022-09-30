#-------------------------#
#--- Runs Testing in R ---#
#-------------------------#


#do this the first time to install the package, then you never have to do it again
#install.packages("tseries")

#do this each time you start an R session
library(tseries)

#Importing data: use .csv format if at all possible
Michelson <- read.csv('Michelson.csv')

# Create a lag plot with lag = 1
lag.plot(Michelson$residual, lags = 1, do.lines = F, labels = F)

# Now set up for the Runs Test

# This line takes the sign of the residuals (+/-) and then converts it into a factor, with values of +1 and -1
x <- factor(sign(Michelson$residual))

# The Runs test by default is a two-sided test.  
# If we want a one-sided test for positive correllation (with fewer runs than would be expected by random chance), set the alternative hypothesis to "less"
runs.test(x, alternative="less")

