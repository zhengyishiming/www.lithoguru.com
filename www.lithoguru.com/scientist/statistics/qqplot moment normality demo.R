#---------------------------------------------------#
#--- Plotting, Moment and Normality Testing in R ---#
#---------------------------------------------------#


#do this the first time to install the package, then you never have to do it again
install.packages("e1071")

#do this each time you start an R session
library(e1071)

#Importing data: use .csv format if at all possible
weights <- read.csv('NBS Weight Data.csv')

#create a histogram
hist(weights$Result, breaks = 12, freq = F, xlab = 'Weight - 10g (mg)', ylab = 'Relative Frequency', main = 'Histogram of NBS Measured Weights')

#create a box and whisker plot
#Box shows Q1 to Q3, with line at median.  IQR = Q3 - Q1
#The top whisker denotes the maximum value or Q3 + 1.5*IQR, whichever is smaller
#The bottom whisker denotes the minimum value or Q1 - 1.5*IQR, whichever is larger.
boxplot(weights$Result, ylab = 'Weight - 10g (mg)', main = '100 NBS Weight Measurements')

#create a normal probability plot
qqout = qqnorm(weights$Result, ylab = 'Weight - 10g (mg)', main = '100 NBS Weight Measurements')
qqline(weights$Result)      #add a straight line to the normal probability plot
cor(qqout$x,qqout$y)    #calculate the correlation coefficient from the qqplot


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

#performs the Shapiro-Wilk test for normality
shapiro.test(weights$Result)
#reject the null hypothesis that the data is normally distributed if the 
#p-value is less that your significance level (e.g., 0.05)
