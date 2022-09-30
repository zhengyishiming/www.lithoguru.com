#----------------------------#
#--- Outlier Testing in R ---#
#----------------------------#


#do this the first time to install the package, then you never have to do it again
#install.packages("e1071")
#install.packages("outliers")

#do this each time you start an R session
library(e1071)
library(outliers)

#Here are the steps we will employ for outlier detection:
#Step 1:  graphing (get an impression of the data)
#Assuming we suspect an underlying Nomral distribution,
#Step 2:  Skewness and Kurtosis tests (do we have outliers?)
#Step 3:  Grubbs test




#Importing data: use .csv format if at all possible
weights <- read.csv('NBS Weight Data.csv')

#Detecting Outliers, step 1:  Graphs
#create a histogram
hist(weights$Result, breaks = 12, freq = F, xlab = 'Weight - 10g (mg)', ylab = 'Relative Frequency', main = 'Histogram of NBS Measured Weights')

#create a box and whisker plot
boxplot(weights$Result, ylab = 'Weight - 10g (mg)', main = '100 NBS Weight Measurements')

#create a normal probability plot
qqout = qqnorm(weights$Result, ylab = 'Weight - 10g (mg)', main = '100 NBS Weight Measurements')
qqline(weights$Result)      #add a straight line to the normal probability plot

#Step 2:  skewness and Kurtosis testing
#this generates G1, the unbiased estimator for skewness
G1 = skewness(weights$Result, na.rm = TRUE, type = 2)
#type 1 = biased estimator, type 2 = unbiased estimator

#Now lets generate SE of the unbiased skewness estimator
n = length(weights$Result)
SE_G1 = sqrt(6*n*(n-1)/(n-2)/(n+1)/(n+3))

z1 = G1/SE_G1
p = 2*(1-pnorm(z1))
p
# the function pnorm(z1) produces the area under the normal distribution below z1

#this generates G2, the unbiased estimator for kurtosis
G2 = kurtosis(weights$Result, na.rm = TRUE, type = 2)
#type 1 = biased estimator, type 2 = unbiased estimator

#Now lets generate SE of the unbiased skewness estimator
n = length(weights$Result)
SE_G2 = 2*SE_G1*sqrt((n*n-1)/(n-3)/(n+5))

z2 = G2/SE_G2
p = 2*(1-pnorm(z2))
p

#From these tests, we might have one or more outliers (in particular, heavy tails).


#Lets test the most extreme values
grubbs.test(weights$Result, type=10)
grubbs.test(weights$Result, type=10, opposite = TRUE)
grubbs.test(weights$Result, type=11)
#type=10 is a test for one outlier (side is detected automatically and can be reversed by opposite parameter). 
#type=11 is a test for two outliers on opposite tails
#type=20 is test for two outliers in one tail (but limited to n < 31).
#Note that this test uses the sample estimate for the standard deviation (divide by n-1).


