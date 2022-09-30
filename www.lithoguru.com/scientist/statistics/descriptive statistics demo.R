#---------------------------------#
#---- Descriptive Statistics -----#
#---------------------------------#


#Put cursor on the line you want to evaluate, then push the
#Run button (or press crtl+R on a PC)
#clear the console using Ctrl+L (on a PC)

#airquality is a built-in data set of R
head(airquality)  #Show the first 6 rows of the data table

#extract the Ozone column of data
ozone = airquality$Ozone
# sample size of "ozone" - be careful, R is case sensitive!
length(ozone)
#Look at the summary statistics - these two functions use different methods
#for calculating Q1 and Q3!!
summary(ozone)
fivenum(ozone)

#To find out how many data points are not "NA"
length(ozone[is.na(ozone) == F])

#look what happens when you try to calcualte the mean:
mean(ozone)
#To remove the na values, set na.rm to be true
mean(ozone, na.rm = T)
var(ozone, na.rm = T)
sd(ozone, na.rm = T)

#To make things easier, let's create a data set with no missing values
ozone1 = ozone[!is.na(ozone)] 
mean(ozone1)


#let's do some plotting of the data
hist(ozone1)        #creates a generic histogram
hist(ozone1, breaks = 12, freq = F, xlab = 'Ozone (ppb)', ylim = c(0, 0.025), ylab = 'Relative Frequency', main = 'Histogram of Ozone Pollution Data')

boxplot(ozone)
#Box shows Q1 to Q3, with line at median.  IQR = Q3 - Q1
#The top whisker denotes the maximum value or Q3 + 1.5*IQR, whichever is smaller
#The bottom whisker denotes the minimum value or Q1 - 1.5*IQR, whichever is larger.
boxplot(ozone, ylab = 'Ozone (ppb)', main = 'Ozone in New York City')
#to find out details of this command:
?boxplot

qqnorm(ozone1)      #creates a normal probability plot
qqline(ozone1)      #adds a straight line to the normal probability plot

