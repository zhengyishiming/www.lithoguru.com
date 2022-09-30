#-------------------------#
#--- Introduction to R ---#
#-------------------------#


#Put cursor on the line you want to evaluate, then push the Run button 
#(or press Crtl+R on a PC)
#Highlight multiple lines to run them in sequence.
#numerical outputs are displayed in the console
#variables are shown in workspace (environment)
#Graphs and help are shown in a separate area

#Submit a line to send to the Console to evaluate:
5+2

#Comments: R ignores any line starting with a '#'
#5+2


#A few built-in functions:
abs(-5)
sqrt(25)

log(5)  #this is the log base-e (natural log)
log(5,2)  #this is the log base-2
exp(1)

round(exp(1),2)

cos(pi)   #pi is a predefined constant
sin(pi)

#
#--- Assigning Objects ---#
#

#Use the assign arrow <- to define objects

#Values:
a <- 4
a = 4  #this does the same thing as a <- 4
a + 10  #this performs the calculation, but doesn't change the stored value of a
a
A + 2  #Note: R is case sensitive!!

#Vectors: a series of values that are all the same type
#(number, character, or logical)

#Numeric vectors (c means concatenate):
x1 <- c(1,2,3,4,5)
#these mathematical operations are performed on each element separately
x1 + 100
(x1 + 1)/2
x1^2
y1 = x1^2
y1

#Sequences and reps 
x2 <- 1:10   #colon gives all numbers between
#sequence of numbers from 0 to 100 in increments of 25
x3 <- seq(0,100,25)
x4 <- rep(-1,10)  #repeat -1 ten times

?rep   #access the help file in this way

#Charater vectors:
y1 <- c('small','medium','large')

#Logical vectors
z1 <- c(TRUE,TRUE,FALSE)


#Some useful vector functions:
sum(x2)  #add up all the values in X2
mean(x2)  #the mean of all the numbers in x2
sd(x2)    #the standard deviation of the numbers in x2
fivenum(x2)  #the five number summary for x2

length(y1)  #the number of items in y1
table(y1)   #creates a frequency table



#
#--- Workspace/Environment management ---#
#

#Lists objects currently in workspace (environment):
ls()

#Removes a specified object from workspace (environment):
rm(list='x1')

#Will clear entire workspace (environment):
rm(list=ls())

#you can also use Ctrl+l in windows to clear the console

#
#
#References:
#
#Quick R - http://www.statmethods.net
#
#Color Brewer - http://colorbrewer2.org/
#
#R Bloggers - http://www.r-bloggers.com
#