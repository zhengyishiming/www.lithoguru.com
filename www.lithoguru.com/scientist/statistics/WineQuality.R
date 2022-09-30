#------------------------------------------------------#
#------- Data 2 Decisions 2018 Exam #2 Solution -------#
#------------------------------------------------------#


# Packages needed for the multigraphsummary routine:
# install.packages("MASS")
# install.packages("e1071")
# install.packages("car")
# install.packages("lmtest")
# install.packages("lawstat")

multigraphsummary <- function(model){
  
  # This routine produces a multigraph summary of a regression model, plus
  # various test of the OLS assumptions.
  
  # First, let's create data frame from the model object
  
  N = nobs(model)
  p = length(model$coefficients)
  
  # Grab the response (y) and predictor (x) values and put them into a data frame
  MyData = matrix(nrow = N, ncol = p)
  MyData = data.frame(MyData)
  MyData[,1] = model$model[,1]
  names(MyData)[1] = paste(attributes(model$terms)$variable[2])
  for (i in 2:p){
    MyData[,i] = model$model[,i]
    names(MyData)[i] = attributes(model$terms)$term.labels[i-1]
  }
  
  y = MyData[,1]
  x = MyData[,2]
  
  
  
  cat("------------------------", "\n")
  cat("Multigraph Regression Summary and Diagnostics", "\n")

  # turn off significance stars
  options(show.signif.stars=F)
  
#-------------------------------------------------------------------------  
  print(summary(model))
  print(confint(model, level=0.95))
  
  # Akiake Information Criterion and Schwarz's Bayesian Criterion
  cat("\n")
  cat("AIC =", AIC(model, k=2),"    BIC =", AIC(model, k=log(N)),  "\n")

  # PRESS: predicted residual error sum of squares
  pr <- residuals(model)/(1 - lm.influence(model)$hat)
  PRESS <- sum(pr^2)
  
  my.anova <- anova(model)
  tss <- sum(my.anova$"Sum Sq")
  # predictive R^2
  pred.r.squared <- 1 - PRESS/(tss)
  cat("PRESS =", PRESS, "     Predictive R^2 =", pred.r.squared, "\n")
  
  cat("------------------------", "\n")

  #-------------------------------------------------------------------------
  # Check for multicollinearity of p > 2
  
  if (p>2) {
    cat("Checking for Multicollinearity", "\n")
    cat("Correlation Matrix:", "\n")
    print(cor(MyData))
    
    cat("Eigenvalues:", "\n")
    print(eigen(cor(MyData))$values)

    # Condition Number: Ratio of max to min Eigen values of the correlation matrix
    cat("Condition Number (is it large, > 100 or so?):", "\n")
    print(kappa(cor(MyData), exact = TRUE))
    
    library(car)
    cat("Variance Inflation Factors (>4=start worrying; >10=do something?):", "\n")
    print(vif(model))
    cat("Is the mean VIF much bigger than 1?", "\n")
    cat("mean VIF = ", mean(vif(model)),  "\n")
    
    cat("------------------------", "\n")
  }
  
  
  #------------------------------------------------------------------------- 
  # Generate some basic plots of the data

    if (p>2) {
    plot(MyData)
  }
  
  # set up so that four graphs appear on each page
  par(mfrow=c(2,2))
  
  if (p==2) {
    
    # plot the data and the best fit model
    ylabel = paste(attributes(model$terms)$variable[2])
    xlabel = attributes(model$terms)$term.labels[1]
    plot(y ~ x, cex = 1.3, main = "Data and Model", xlab=xlabel, ylab=ylabel)

    # now add the model line and CI to the plot
    # create a new data frame that is a simple array of x-values
    newx = seq(min(x),max(x),length.out = 101)
    new <- data.frame(x = newx)
    names(new) = xlabel
    
    y_hat = predict(model, newdata=new, interval="confidence", level = 0.95, type="response")
    lines(newx, y_hat[,1], col = "red")
    # Note: this shows the confidence intervals, not the prediction intervals
    lines(newx, y_hat[,2], col = "blue", lty=2)
    lines(newx, y_hat[,3], col = "blue", lty=2)
    
    # add a sub-title to the graph that shows the fit R^2
    mylabel = paste("R^2 = ",format(summary(model)$r.squared, digits = 3))
    mtext(mylabel)
  }
  
  #plot the predicted y-values versus the measured y-values
  # install.packages("pls")
  plot(fitted(model) ~ y, cex = 1.3, main = "Predicted Response", xlab="Measured Y", ylab="Predicted Y")
  abline(a=0,b=1,col='red')  # a y=x line
  
  # plot the residuals
  library(MASS)  #for the studres function
  esr = studres(model) # externally studentized residuals
  plot(esr~predict(model), main = "Residuals", xlab="Predicted Response")
  abline(h=0,col='red')

  #------------------------------------------------------------------------- 
  # Testing for Outliers
  
  #create a box and whisker plot
  #Box shows Q1 to Q3, with line at median.  IQR = Q3 - Q1
  #The top whisker denotes the min(maximum value, Q3 + 1.5*IQR)
  #The bottom whisker denotes the max(minimum value, Q1 - 1.5*IQR)
  library(e1071)
  boxplot(esr, ylab = "esr", main = "Box & Wiskers")

  cat("Testing for Outliers:", "\n")

  alpha = 0.01
  tinv = qt(alpha/2/N,N-p-1)
  Grubbs_crit = (N-1)/sqrt(N)*sqrt(tinv^2/(N-3+tinv^2))

  cat("Grubbs' Critical Value (alpha = 0.01) =", Grubbs_crit, "\n")
  # Now test for one outlier
  library(car)
  # Bonferonni p-value (Grubbs test) for most extreme observation
  cat("Grubbs' test for one outlier using outlierTest from the car package:", "\n")
  print(outlierTest(model))
  
  cat("------------------------", "\n")
  
  #------------------------------------------------------------------------- 
  # Testing for Normality
  
  # histogram of externally studentized residuals
  hist(studres(model), freq=FALSE, main="Distribution of esr", col="grey", xlab = "esr")
  #add a standard normal curve on top of the historgram
  xfit<-seq(min(studres(model)),max(studres(model)),length=40) 
  yfit<-dnorm(xfit) 
  lines(xfit, yfit)
  
  # Normal Probability Plot
  qqout = qqnorm(esr)
  qqline(esr, col = "red")
  # calculate the correlation coefficient from the qqplot
  qq_correlation = cor(qqout$x,qqout$y)
  mylabel = paste("r = ",format(qq_correlation, digits = 4))
  text(-1.5, 2,labels = mylabel) 
  
  #Testing for normality
  cat("Testing Externally Studentized Residuals (esr) for Normality:", "\n")

  library(e1071)
  #this generates G1, the unbiased estimator for skewness
  G1 = skewness(esr, na.rm = TRUE, type = 2)
  #type 1 = biased estimator, type 2 = unbiased estimator
  
  #Now let's calculate the standard error of the unbiased skewness estimator
  SE_G1 = sqrt(6*N*(N-1)/(N-2)/(N+1)/(N+3))
  
  z1 = G1/SE_G1
  if (z1 > 0) {
    pvalue = 2*(1-pnorm(z1))
  } else {
    pvalue = 2*pnorm(z1)
  }
  
  cat("(Small p-values means we can reject the assumption of esr normality)", "\n")
  cat("Skewness Z = ",format(z1, digits = 4), ",   p-value = ", pvalue, "\n")
  
  #this generates G2, the unbiased estimator for kurtosis
  G2 = kurtosis(esr, na.rm = TRUE, type = 2)
  #type 1 = biased estimator, type 2 = unbiased estimator
  
  #Now let's calculate the standard error of the unbiased skewness estimator
  SE_G2 = 2*SE_G1*sqrt((N*N-1)/(N-3)/(N+5))
  
  z2 = G2/SE_G2
  if (z2 > 0) {
    pvalue = 2*(1-pnorm(z2))
  } else {
    pvalue = 2*pnorm(z2)
  }
  
  cat("Kurtosis Z = ",format(z2, digits = 4), ",   p-value = ",pvalue, "\n")
  
  #performs the Shapiro-Wilk test for normality
  print(shapiro.test(esr))
  #reject the null hypothesis that the data is normally distributed if the 
  #p-value is less that your significance level (e.g., 0.05)
  
  cat("------------------------", "\n")

  #------------------------------------------------------------------------- 
  # Testing for Influence
  
  cat("Testing Externally Studentized Residuals (esr) for Influence:", "\n")
  
  # plotting options specifically for lm objects
  # plot(model, which=4, cook.levels=cutoff)
  # 1 = residuals versus fitted
  # 2 = QQ plot
  # 3 = Scale-Location
  # 4 = Cook's Distance
  # 5 = Williams-like Graph
  
  # Let's create a Williams Graph
  norm_leverage = (length(esr)/2)*hatvalues(model)
  plot(abs(esr) ~ norm_leverage, xlab = "leverage*n/p", ylab = "|esr|", main ="Williams Graph")
  
  # add the critical lines to the Williams Graph
  abline(h=Grubbs_crit, col="red") # draw a red horizontal line at Gcrit
  abline(v=2, col="red") # draw a red vertical line at 2
  
  D = cooks.distance(model) #this provides the cook's Distance for each data points
  cutoff <- 4/summary(model)$df[2]
  cat("Cook's Distance cut-off (4/df) = ", cutoff, "\n")
  cat("Maximum Cook's Distance = ", max(D), "\n")
  cat("  which occurs at index = ", which.max(D), "\n")
  
  # we can also calculate DFFITS and DFBETA
  plot(D ~ fitted(model), ylab = "Cook's D", main = "Cook's Distance", xlab="Predicted Response")
  # For Cook's D plot, identify D values > 4/DF
  abline(h=cutoff, col="red")
  xrange = max(fitted(model)) - min(fitted(model))
  x_text = max(fitted(model)) - 0.1*xrange
  y_text = cutoff + 0.05*max(D)
  text(x_text, y_text, "D = 4/df", col = "red")
  
  
  dff = dffits(model)
  cat("Maximum DFFITS = ", max(abs(dff)), "\n")
  cat("  which occurs at index = ", which.max(abs(dff)), "\n")

  plot(dff ~ fitted(model), ylab = "DFFITS", main = "DFFITS", xlab="Predicted Response")
  
  dfb = dfbeta(model)[,'(Intercept)'] 
  cat("Intercept Maximum DFBETA = ", max(abs(dfb)), "\n")
  cat("  which occurs at index = ", which.max(abs(dfb)), "\n")
  
  plot(dfb ~ fitted(model), ylab = "DFBETA", main = "Intercept DFBETA", xlab="Predicted Response")

  for (i in 2:p){
    dfb = dfbeta(model)[,i]  
    label = cat(attributes(model$terms)$term.labels[i-1], "Maximum DFBETA")
    cat(label, "= ", max(abs(dfb)), "\n")
    cat("  which occurs at index = ", which.max(abs(dfb)), "\n")

    mainlabel = paste(attributes(model$terms)$term.labels[i-1], "DFBETA")
    plot(dfb ~ fitted(model), ylab = "DFBETA", main = mainlabel, xlab="Predicted Response")
    }

    cat("------------------------", "\n")
  
  #------------------------------------------------------------------------- 
  # Testing Externally Studentized Residuals (esr) for Homoscedasticity
  
  cat("Testing for Homoscedasticity:", "\n")
  
  cat("(Data sorted by y-hat and split in half)", "\n")
  data_sorted = data.frame(cbind(fitted(model),studres(model)))
  attach(data_sorted)
  data_sorted = data_sorted[order(X1),]
  detach(data_sorted)
  esr_sorted = data_sorted[,2]
  
  cat("(Small p-value indicates heteroscedasticity)", "\n", "\n")

  # Breusch-Pagan test for nonconstant variance
  library(lmtest)
  cat("Breusch-Pagan test from bptest, lmtest package:", "\n")
  print(bptest(model))  #from lmtest package, defaults to to testing against explanatory variables
  
  # split the data in half
  N = length(esr_sorted)
  group = rep(2,N)
  group[1:N/2] = 1
  
  cat("Barlett test from bartlett.test, stats package:", "\n")
  print(bartlett.test(esr_sorted, as.factor(group)))
  # If the p-value is less than alpha, we reject the null hypothesis that
  # the variances of each group are equal.
  
  library(lawstat)
  cat("Brown-Forsythe test from levene.test, lawstat package:", "\n")
  print(levene.test(esr_sorted, as.factor(group)))
  # Note that this test is formualted using an F statistic, rather than the
  # t-statistic that was discussed in class.  They work out the same, giving
  # the same p-value.
  
  cat("------------------------", "\n")
  
  #------------------------------------------------------------------------- 
  # Testing for Homoscedasticity
  
  
  rho = acf(model$residuals, main = "Autocorrelation Function")
  r = rho[1]$acf[1,,]
  
  # manual creation of a lag plot
  n = length(model$residuals)
  yprime = rep(0,n-1)
  xprime = rep(0,n-1)
  for (i in 1:n-1){
    yprime[i] = model$residuals[i+1]
    xprime[i] = model$residuals[i]
  }
  
  plot(yprime ~ xprime, main = "Lag 1 Plot", xlab = "residual lag 1", ylab = "residual")
  
  cat("Testing for Autocorrelated Residuals:", "\n")
  cat("(only useful if data is in some natural order, such as time)", "\n")
  
  
  #Durbin-Watson Test
  library(lmtest)
  print(dwtest(model))
  
  cat("Estimated AR(1) parameter r = ", r, "\n")
  
  cat("------------------------", "\n")
  
  return
}


#Importing data: use .csv format if at all possible
WineQuality <- read.csv('WineQuality.csv')

# best main factor model
model1 = lm(quality~volatile.acidity + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + pH + sulphates + alcohol, data = WineQuality)

multigraphsummary(model1)

print(summary(model1))
cat("Model 1 AIC =", AIC(model1, k=2), "\n")

# best main factor + interaction model (^2)
model2 = lm(quality~fixed.acidity + volatile.acidity + citric.acid + 
        chlorides + free.sulfur.dioxide + total.sulfur.dioxide + 
        pH + sulphates + alcohol + fixed.acidity:citric.acid + 
        fixed.acidity:chlorides + fixed.acidity:free.sulfur.dioxide + 
        fixed.acidity:pH + fixed.acidity:sulphates + fixed.acidity:alcohol + 
        volatile.acidity:citric.acid + volatile.acidity:free.sulfur.dioxide + 
        volatile.acidity:total.sulfur.dioxide + volatile.acidity:alcohol + 
        citric.acid:total.sulfur.dioxide + citric.acid:density + 
        citric.acid:pH + citric.acid:alcohol + residual.sugar:total.sulfur.dioxide + 
        chlorides:free.sulfur.dioxide + chlorides:density + free.sulfur.dioxide:total.sulfur.dioxide + 
        free.sulfur.dioxide:density + free.sulfur.dioxide:sulphates + 
        free.sulfur.dioxide:alcohol + total.sulfur.dioxide:density + 
        total.sulfur.dioxide:alcohol + density:sulphates + pH:sulphates, data = WineQuality)

multigraphsummary(model2)

print(summary(model2))
cat("Model 2 AIC =", AIC(model2, k=2), "\n")

# best main factor + two interaction model (^3)
model3 = lm(quality ~ fixed.acidity + volatile.acidity + citric.acid + residual.sugar + 
      chlorides + free.sulfur.dioxide + total.sulfur.dioxide + 
      pH + sulphates + alcohol + fixed.acidity:volatile.acidity + 
      fixed.acidity:citric.acid + fixed.acidity:residual.sugar + 
      fixed.acidity:chlorides + fixed.acidity:free.sulfur.dioxide + 
      fixed.acidity:total.sulfur.dioxide + fixed.acidity:density + 
      fixed.acidity:pH + fixed.acidity:alcohol + 
      volatile.acidity:citric.acid + volatile.acidity:residual.sugar + 
      volatile.acidity:chlorides + volatile.acidity:free.sulfur.dioxide + 
      volatile.acidity:total.sulfur.dioxide + volatile.acidity:density + 
      volatile.acidity:sulphates + volatile.acidity:alcohol + 
      citric.acid:residual.sugar + citric.acid:chlorides + citric.acid:free.sulfur.dioxide + 
      citric.acid:total.sulfur.dioxide + citric.acid:density + 
      citric.acid:pH + citric.acid:sulphates + citric.acid:alcohol + 
      residual.sugar:chlorides + residual.sugar:free.sulfur.dioxide + 
      residual.sugar:total.sulfur.dioxide + residual.sugar:density + 
      residual.sugar:pH + residual.sugar:sulphates + residual.sugar:alcohol + 
      chlorides:free.sulfur.dioxide + 
      chlorides:density + chlorides:pH + chlorides:sulphates + 
      chlorides:alcohol + free.sulfur.dioxide:total.sulfur.dioxide + 
      free.sulfur.dioxide:density + free.sulfur.dioxide:pH + free.sulfur.dioxide:sulphates + 
      free.sulfur.dioxide:alcohol + total.sulfur.dioxide:density + 
      total.sulfur.dioxide:pH + total.sulfur.dioxide:sulphates + 
      total.sulfur.dioxide:alcohol + density:pH + density:sulphates + 
      density:alcohol + pH:sulphates + 
      fixed.acidity:citric.acid:density + fixed.acidity:citric.acid:pH + 
      fixed.acidity:residual.sugar:free.sulfur.dioxide + fixed.acidity:residual.sugar:total.sulfur.dioxide + 
      fixed.acidity:residual.sugar:density + fixed.acidity:chlorides:density + 
      fixed.acidity:chlorides:sulphates + fixed.acidity:chlorides:alcohol + 
      fixed.acidity:free.sulfur.dioxide:total.sulfur.dioxide + 
      fixed.acidity:free.sulfur.dioxide:density + fixed.acidity:free.sulfur.dioxide:pH + 
      fixed.acidity:free.sulfur.dioxide:sulphates + fixed.acidity:total.sulfur.dioxide:density + 
      fixed.acidity:total.sulfur.dioxide:alcohol + fixed.acidity:density:pH + 
      fixed.acidity:density:alcohol + volatile.acidity:citric.acid:residual.sugar + 
      volatile.acidity:citric.acid:chlorides + volatile.acidity:citric.acid:total.sulfur.dioxide + 
      volatile.acidity:citric.acid:alcohol + volatile.acidity:residual.sugar:chlorides + 
      volatile.acidity:residual.sugar:alcohol + volatile.acidity:chlorides:pH + 
      volatile.acidity:chlorides:alcohol + volatile.acidity:free.sulfur.dioxide:pH + 
      volatile.acidity:free.sulfur.dioxide:alcohol + volatile.acidity:density:sulphates + 
      volatile.acidity:pH:sulphates + volatile.acidity:sulphates:alcohol + 
      citric.acid:residual.sugar:chlorides + citric.acid:residual.sugar:free.sulfur.dioxide + 
      citric.acid:residual.sugar:total.sulfur.dioxide + citric.acid:chlorides:free.sulfur.dioxide + 
      citric.acid:free.sulfur.dioxide:density + citric.acid:total.sulfur.dioxide:density + 
      citric.acid:total.sulfur.dioxide:pH + citric.acid:total.sulfur.dioxide:alcohol + 
      citric.acid:density:pH + citric.acid:density:alcohol + citric.acid:pH:sulphates + 
      citric.acid:pH:alcohol + residual.sugar:chlorides:density + 
      residual.sugar:chlorides:alcohol + residual.sugar:free.sulfur.dioxide:density + 
      residual.sugar:free.sulfur.dioxide:alcohol + residual.sugar:total.sulfur.dioxide:density + 
      residual.sugar:total.sulfur.dioxide:alcohol + residual.sugar:density:pH + 
      residual.sugar:sulphates:alcohol + chlorides:free.sulfur.dioxide:total.sulfur.dioxide + 
      chlorides:density:pH + chlorides:density:sulphates + chlorides:density:alcohol + 
      chlorides:pH:sulphates + free.sulfur.dioxide:total.sulfur.dioxide:density + 
      free.sulfur.dioxide:density:pH + free.sulfur.dioxide:density:sulphates + 
      free.sulfur.dioxide:density:alcohol + free.sulfur.dioxide:pH:alcohol + 
      free.sulfur.dioxide:sulphates:alcohol + total.sulfur.dioxide:density:pH + 
      total.sulfur.dioxide:density:sulphates + total.sulfur.dioxide:density:alcohol + 
      total.sulfur.dioxide:pH:sulphates + total.sulfur.dioxide:pH:alcohol + 
      total.sulfur.dioxide:sulphates:alcohol + density:pH:sulphates + 
      fixed.acidity:residual.sugar:chlorides, data = WineQuality)

multigraphsummary(model3)

print(summary(model3))
cat("Model 3 AIC =", AIC(model3, k=2), "\n")

# best main factor + two interaction model (^3)
model4 = lm(quality ~ fixed.acidity + volatile.acidity + citric.acid + residual.sugar + 
              chlorides + free.sulfur.dioxide + total.sulfur.dioxide + 
              density + pH + sulphates + alcohol + fixed.acidity:volatile.acidity + 
              fixed.acidity:citric.acid + fixed.acidity:residual.sugar + 
              fixed.acidity:chlorides + fixed.acidity:free.sulfur.dioxide + 
              fixed.acidity:total.sulfur.dioxide + fixed.acidity:density + 
              fixed.acidity:pH + fixed.acidity:sulphates + fixed.acidity:alcohol + 
              volatile.acidity:citric.acid + volatile.acidity:residual.sugar + 
              volatile.acidity:chlorides + volatile.acidity:free.sulfur.dioxide + 
              volatile.acidity:total.sulfur.dioxide + volatile.acidity:density + 
              volatile.acidity:pH + volatile.acidity:sulphates + volatile.acidity:alcohol + 
              citric.acid:residual.sugar + citric.acid:chlorides + citric.acid:free.sulfur.dioxide + 
              citric.acid:total.sulfur.dioxide + citric.acid:density + 
              citric.acid:pH + citric.acid:sulphates + citric.acid:alcohol + 
              residual.sugar:chlorides + residual.sugar:free.sulfur.dioxide + 
              residual.sugar:total.sulfur.dioxide + residual.sugar:density + 
              residual.sugar:pH + residual.sugar:sulphates + residual.sugar:alcohol + 
              chlorides:free.sulfur.dioxide + chlorides:total.sulfur.dioxide + 
              chlorides:density + chlorides:pH + chlorides:sulphates + 
              chlorides:alcohol + free.sulfur.dioxide:total.sulfur.dioxide + 
              free.sulfur.dioxide:density + free.sulfur.dioxide:pH + free.sulfur.dioxide:sulphates + 
              free.sulfur.dioxide:alcohol + total.sulfur.dioxide:density + 
              total.sulfur.dioxide:pH + total.sulfur.dioxide:sulphates + 
              total.sulfur.dioxide:alcohol + density:pH + density:sulphates + 
              density:alcohol + pH:sulphates + pH:alcohol + sulphates:alcohol + 
              fixed.acidity:volatile.acidity:citric.acid + fixed.acidity:volatile.acidity:residual.sugar + 
              fixed.acidity:volatile.acidity:chlorides + fixed.acidity:volatile.acidity:free.sulfur.dioxide + 
              fixed.acidity:volatile.acidity:total.sulfur.dioxide + fixed.acidity:volatile.acidity:density + 
              fixed.acidity:volatile.acidity:pH + fixed.acidity:volatile.acidity:sulphates + 
              fixed.acidity:volatile.acidity:alcohol + fixed.acidity:citric.acid:residual.sugar + 
              fixed.acidity:citric.acid:chlorides + fixed.acidity:citric.acid:free.sulfur.dioxide + 
              fixed.acidity:citric.acid:total.sulfur.dioxide + fixed.acidity:citric.acid:density + 
              fixed.acidity:citric.acid:pH + fixed.acidity:citric.acid:sulphates + 
              fixed.acidity:citric.acid:alcohol + fixed.acidity:residual.sugar:chlorides + 
              fixed.acidity:residual.sugar:free.sulfur.dioxide + fixed.acidity:residual.sugar:total.sulfur.dioxide + 
              fixed.acidity:residual.sugar:density + fixed.acidity:residual.sugar:pH + 
              fixed.acidity:residual.sugar:sulphates + fixed.acidity:residual.sugar:alcohol + 
              fixed.acidity:chlorides:free.sulfur.dioxide + fixed.acidity:chlorides:total.sulfur.dioxide + 
              fixed.acidity:chlorides:density + fixed.acidity:chlorides:pH + 
              fixed.acidity:chlorides:sulphates + fixed.acidity:chlorides:alcohol + 
              fixed.acidity:free.sulfur.dioxide:total.sulfur.dioxide + 
              fixed.acidity:free.sulfur.dioxide:density + fixed.acidity:free.sulfur.dioxide:pH + 
              fixed.acidity:free.sulfur.dioxide:sulphates + fixed.acidity:free.sulfur.dioxide:alcohol + 
              fixed.acidity:total.sulfur.dioxide:density + fixed.acidity:total.sulfur.dioxide:pH + 
              fixed.acidity:total.sulfur.dioxide:sulphates + fixed.acidity:total.sulfur.dioxide:alcohol + 
              fixed.acidity:density:pH + fixed.acidity:density:sulphates + 
              fixed.acidity:density:alcohol + fixed.acidity:pH:sulphates + 
              fixed.acidity:pH:alcohol + fixed.acidity:sulphates:alcohol + 
              volatile.acidity:citric.acid:residual.sugar + volatile.acidity:citric.acid:chlorides + 
              volatile.acidity:citric.acid:free.sulfur.dioxide + volatile.acidity:citric.acid:total.sulfur.dioxide + 
              volatile.acidity:citric.acid:density + volatile.acidity:citric.acid:pH + 
              volatile.acidity:citric.acid:sulphates + volatile.acidity:citric.acid:alcohol + 
              volatile.acidity:residual.sugar:chlorides + volatile.acidity:residual.sugar:free.sulfur.dioxide + 
              volatile.acidity:residual.sugar:total.sulfur.dioxide + volatile.acidity:residual.sugar:density + 
              volatile.acidity:residual.sugar:pH + volatile.acidity:residual.sugar:sulphates + 
              volatile.acidity:residual.sugar:alcohol + volatile.acidity:chlorides:free.sulfur.dioxide + 
              volatile.acidity:chlorides:total.sulfur.dioxide + volatile.acidity:chlorides:density + 
              volatile.acidity:chlorides:pH + volatile.acidity:chlorides:sulphates + 
              volatile.acidity:chlorides:alcohol + volatile.acidity:free.sulfur.dioxide:density + 
              volatile.acidity:free.sulfur.dioxide:pH + volatile.acidity:free.sulfur.dioxide:sulphates + 
              volatile.acidity:free.sulfur.dioxide:alcohol + volatile.acidity:total.sulfur.dioxide:density + 
              volatile.acidity:total.sulfur.dioxide:pH + volatile.acidity:total.sulfur.dioxide:sulphates + 
              volatile.acidity:total.sulfur.dioxide:alcohol + volatile.acidity:density:pH + 
              volatile.acidity:density:sulphates + volatile.acidity:density:alcohol + 
              volatile.acidity:pH:sulphates + volatile.acidity:pH:alcohol + 
              volatile.acidity:sulphates:alcohol + citric.acid:residual.sugar:chlorides + 
              citric.acid:residual.sugar:free.sulfur.dioxide + citric.acid:residual.sugar:total.sulfur.dioxide + 
              citric.acid:residual.sugar:density + citric.acid:residual.sugar:pH + 
              citric.acid:residual.sugar:sulphates + citric.acid:residual.sugar:alcohol + 
              citric.acid:chlorides:free.sulfur.dioxide + citric.acid:chlorides:total.sulfur.dioxide + 
              citric.acid:chlorides:density + citric.acid:chlorides:pH + 
              citric.acid:chlorides:sulphates + citric.acid:chlorides:alcohol + 
              citric.acid:free.sulfur.dioxide:total.sulfur.dioxide + citric.acid:free.sulfur.dioxide:density + 
              citric.acid:free.sulfur.dioxide:pH + citric.acid:free.sulfur.dioxide:sulphates + 
              citric.acid:free.sulfur.dioxide:alcohol + citric.acid:total.sulfur.dioxide:density + 
              citric.acid:total.sulfur.dioxide:pH + citric.acid:total.sulfur.dioxide:sulphates + 
              citric.acid:total.sulfur.dioxide:alcohol + citric.acid:density:pH + 
              citric.acid:density:sulphates + citric.acid:density:alcohol + 
              citric.acid:pH:sulphates + citric.acid:pH:alcohol + citric.acid:sulphates:alcohol + 
              residual.sugar:chlorides:free.sulfur.dioxide + residual.sugar:chlorides:total.sulfur.dioxide + 
              residual.sugar:chlorides:density + residual.sugar:chlorides:pH + 
              residual.sugar:chlorides:sulphates + residual.sugar:chlorides:alcohol + 
              residual.sugar:free.sulfur.dioxide:total.sulfur.dioxide + 
              residual.sugar:free.sulfur.dioxide:density + residual.sugar:free.sulfur.dioxide:pH + 
              residual.sugar:free.sulfur.dioxide:sulphates + residual.sugar:free.sulfur.dioxide:alcohol + 
              residual.sugar:total.sulfur.dioxide:density + residual.sugar:total.sulfur.dioxide:pH + 
              residual.sugar:total.sulfur.dioxide:sulphates + residual.sugar:total.sulfur.dioxide:alcohol + 
              residual.sugar:density:pH + residual.sugar:density:sulphates + 
              residual.sugar:density:alcohol + residual.sugar:pH:sulphates + 
              residual.sugar:pH:alcohol + residual.sugar:sulphates:alcohol + 
              chlorides:free.sulfur.dioxide:total.sulfur.dioxide + chlorides:free.sulfur.dioxide:density + 
              chlorides:free.sulfur.dioxide:pH + chlorides:free.sulfur.dioxide:sulphates + 
              chlorides:free.sulfur.dioxide:alcohol + chlorides:total.sulfur.dioxide:density + 
              chlorides:total.sulfur.dioxide:pH + chlorides:total.sulfur.dioxide:sulphates + 
              chlorides:total.sulfur.dioxide:alcohol + chlorides:density:pH + 
              chlorides:density:sulphates + chlorides:density:alcohol + 
              chlorides:pH:sulphates + chlorides:pH:alcohol + chlorides:sulphates:alcohol + 
              free.sulfur.dioxide:total.sulfur.dioxide:density + free.sulfur.dioxide:total.sulfur.dioxide:pH + 
              free.sulfur.dioxide:total.sulfur.dioxide:sulphates + free.sulfur.dioxide:density:pH + 
              free.sulfur.dioxide:density:sulphates + free.sulfur.dioxide:density:alcohol + 
              free.sulfur.dioxide:pH:sulphates + free.sulfur.dioxide:pH:alcohol + 
              free.sulfur.dioxide:sulphates:alcohol + total.sulfur.dioxide:density:pH + 
              total.sulfur.dioxide:density:sulphates + total.sulfur.dioxide:density:alcohol + 
              total.sulfur.dioxide:pH:sulphates + total.sulfur.dioxide:pH:alcohol + 
              total.sulfur.dioxide:sulphates:alcohol + density:pH:sulphates + 
              density:pH:alcohol + density:sulphates:alcohol + pH:sulphates:alcohol + 
              fixed.acidity:volatile.acidity:citric.acid:chlorides + fixed.acidity:volatile.acidity:citric.acid:free.sulfur.dioxide + 
              fixed.acidity:volatile.acidity:citric.acid:density + fixed.acidity:volatile.acidity:citric.acid:pH + 
              fixed.acidity:volatile.acidity:citric.acid:sulphates + fixed.acidity:volatile.acidity:citric.acid:alcohol + 
              fixed.acidity:volatile.acidity:residual.sugar:chlorides + 
              fixed.acidity:volatile.acidity:residual.sugar:free.sulfur.dioxide + 
              fixed.acidity:volatile.acidity:residual.sugar:total.sulfur.dioxide + 
              fixed.acidity:volatile.acidity:residual.sugar:density + fixed.acidity:volatile.acidity:residual.sugar:sulphates + 
              fixed.acidity:volatile.acidity:residual.sugar:alcohol + fixed.acidity:volatile.acidity:chlorides:total.sulfur.dioxide + 
              fixed.acidity:volatile.acidity:chlorides:alcohol + fixed.acidity:volatile.acidity:free.sulfur.dioxide:density + 
              fixed.acidity:volatile.acidity:free.sulfur.dioxide:pH + fixed.acidity:volatile.acidity:total.sulfur.dioxide:alcohol + 
              fixed.acidity:volatile.acidity:density:pH + fixed.acidity:volatile.acidity:density:alcohol + 
              fixed.acidity:volatile.acidity:pH:alcohol + fixed.acidity:volatile.acidity:sulphates:alcohol + 
              fixed.acidity:citric.acid:residual.sugar:chlorides + fixed.acidity:citric.acid:residual.sugar:free.sulfur.dioxide + 
              fixed.acidity:citric.acid:chlorides:density + fixed.acidity:citric.acid:chlorides:pH + 
              fixed.acidity:citric.acid:chlorides:sulphates + fixed.acidity:citric.acid:chlorides:alcohol + 
              fixed.acidity:citric.acid:free.sulfur.dioxide:total.sulfur.dioxide + 
              fixed.acidity:citric.acid:total.sulfur.dioxide:density + 
              fixed.acidity:citric.acid:total.sulfur.dioxide:sulphates + 
              fixed.acidity:citric.acid:density:sulphates + fixed.acidity:citric.acid:density:alcohol + 
              fixed.acidity:citric.acid:sulphates:alcohol + fixed.acidity:residual.sugar:chlorides:free.sulfur.dioxide + 
              fixed.acidity:residual.sugar:chlorides:density + fixed.acidity:residual.sugar:free.sulfur.dioxide:pH + 
              fixed.acidity:residual.sugar:free.sulfur.dioxide:sulphates + 
              fixed.acidity:residual.sugar:free.sulfur.dioxide:alcohol + 
              fixed.acidity:residual.sugar:density:sulphates + fixed.acidity:residual.sugar:sulphates:alcohol + 
              fixed.acidity:chlorides:free.sulfur.dioxide:sulphates + fixed.acidity:chlorides:pH:alcohol + 
              fixed.acidity:free.sulfur.dioxide:total.sulfur.dioxide:sulphates + 
              fixed.acidity:free.sulfur.dioxide:density:pH + fixed.acidity:free.sulfur.dioxide:density:sulphates + 
              fixed.acidity:free.sulfur.dioxide:pH:alcohol + fixed.acidity:free.sulfur.dioxide:sulphates:alcohol + 
              fixed.acidity:total.sulfur.dioxide:density:pH + fixed.acidity:total.sulfur.dioxide:density:alcohol + 
              fixed.acidity:total.sulfur.dioxide:pH:sulphates + fixed.acidity:density:pH:sulphates + 
              fixed.acidity:density:pH:alcohol + fixed.acidity:pH:sulphates:alcohol + 
              volatile.acidity:citric.acid:residual.sugar:chlorides + volatile.acidity:citric.acid:residual.sugar:free.sulfur.dioxide + 
              volatile.acidity:citric.acid:residual.sugar:density + volatile.acidity:citric.acid:residual.sugar:pH + 
              volatile.acidity:citric.acid:residual.sugar:alcohol + volatile.acidity:citric.acid:chlorides:density + 
              volatile.acidity:citric.acid:chlorides:pH + volatile.acidity:citric.acid:chlorides:alcohol + 
              volatile.acidity:citric.acid:total.sulfur.dioxide:density + 
              volatile.acidity:citric.acid:total.sulfur.dioxide:pH + volatile.acidity:citric.acid:density:pH + 
              volatile.acidity:citric.acid:density:sulphates + volatile.acidity:citric.acid:pH:sulphates + 
              volatile.acidity:citric.acid:pH:alcohol + volatile.acidity:citric.acid:sulphates:alcohol + 
              volatile.acidity:residual.sugar:chlorides:density + volatile.acidity:residual.sugar:free.sulfur.dioxide:density + 
              volatile.acidity:residual.sugar:free.sulfur.dioxide:pH + 
              volatile.acidity:residual.sugar:free.sulfur.dioxide:sulphates + 
              volatile.acidity:residual.sugar:free.sulfur.dioxide:alcohol + 
              volatile.acidity:residual.sugar:total.sulfur.dioxide:density + 
              volatile.acidity:residual.sugar:total.sulfur.dioxide:sulphates + 
              volatile.acidity:residual.sugar:total.sulfur.dioxide:alcohol + 
              volatile.acidity:residual.sugar:density:pH + volatile.acidity:residual.sugar:density:alcohol + 
              volatile.acidity:residual.sugar:pH:sulphates + volatile.acidity:chlorides:free.sulfur.dioxide:density + 
              volatile.acidity:chlorides:free.sulfur.dioxide:sulphates + 
              volatile.acidity:chlorides:total.sulfur.dioxide:density + 
              volatile.acidity:chlorides:total.sulfur.dioxide:alcohol + 
              volatile.acidity:chlorides:density:pH + volatile.acidity:chlorides:pH:alcohol + 
              volatile.acidity:free.sulfur.dioxide:density:alcohol + volatile.acidity:free.sulfur.dioxide:pH:sulphates + 
              volatile.acidity:total.sulfur.dioxide:density:sulphates + 
              volatile.acidity:total.sulfur.dioxide:density:alcohol + volatile.acidity:total.sulfur.dioxide:pH:sulphates + 
              volatile.acidity:total.sulfur.dioxide:sulphates:alcohol + 
              volatile.acidity:density:pH:alcohol + citric.acid:residual.sugar:chlorides:free.sulfur.dioxide + 
              citric.acid:residual.sugar:chlorides:total.sulfur.dioxide + 
              citric.acid:residual.sugar:chlorides:density + citric.acid:residual.sugar:chlorides:alcohol + 
              citric.acid:residual.sugar:free.sulfur.dioxide:pH + citric.acid:residual.sugar:free.sulfur.dioxide:sulphates + 
              citric.acid:residual.sugar:total.sulfur.dioxide:pH + citric.acid:residual.sugar:total.sulfur.dioxide:sulphates + 
              citric.acid:residual.sugar:density:sulphates + citric.acid:residual.sugar:sulphates:alcohol + 
              citric.acid:chlorides:free.sulfur.dioxide:pH + citric.acid:chlorides:total.sulfur.dioxide:density + 
              citric.acid:chlorides:total.sulfur.dioxide:sulphates + citric.acid:chlorides:total.sulfur.dioxide:alcohol + 
      citric.acid:chlorides:density:pH + citric.acid:chlorides:density:sulphates + 
              citric.acid:chlorides:density:alcohol + citric.acid:chlorides:pH:sulphates + 
              citric.acid:chlorides:pH:alcohol + citric.acid:free.sulfur.dioxide:total.sulfur.dioxide:pH + 
              citric.acid:free.sulfur.dioxide:density:pH + citric.acid:free.sulfur.dioxide:density:sulphates + 
              citric.acid:free.sulfur.dioxide:pH:sulphates + citric.acid:free.sulfur.dioxide:pH:alcohol + 
              citric.acid:free.sulfur.dioxide:sulphates:alcohol + citric.acid:total.sulfur.dioxide:density:pH + 
              citric.acid:total.sulfur.dioxide:density:sulphates + citric.acid:total.sulfur.dioxide:pH:alcohol + 
              citric.acid:density:pH:sulphates + citric.acid:density:sulphates:alcohol + 
              residual.sugar:chlorides:free.sulfur.dioxide:density + residual.sugar:chlorides:free.sulfur.dioxide:pH + 
              residual.sugar:chlorides:free.sulfur.dioxide:alcohol + residual.sugar:chlorides:total.sulfur.dioxide:sulphates + 
      residual.sugar:chlorides:density:alcohol + residual.sugar:chlorides:pH:sulphates + 
      residual.sugar:free.sulfur.dioxide:total.sulfur.dioxide:pH + 
      residual.sugar:free.sulfur.dioxide:total.sulfur.dioxide:sulphates + 
      residual.sugar:free.sulfur.dioxide:density:sulphates + residual.sugar:free.sulfur.dioxide:density:alcohol + 
      residual.sugar:free.sulfur.dioxide:pH:sulphates + residual.sugar:free.sulfur.dioxide:pH:alcohol + 
      residual.sugar:free.sulfur.dioxide:sulphates:alcohol + residual.sugar:total.sulfur.dioxide:density:pH + 
      residual.sugar:total.sulfur.dioxide:density:sulphates + residual.sugar:total.sulfur.dioxide:pH:sulphates + 
      residual.sugar:total.sulfur.dioxide:pH:alcohol + residual.sugar:total.sulfur.dioxide:sulphates:alcohol + 
      residual.sugar:density:pH:sulphates + residual.sugar:density:pH:alcohol + 
      residual.sugar:density:sulphates:alcohol + residual.sugar:pH:sulphates:alcohol + 
      chlorides:free.sulfur.dioxide:total.sulfur.dioxide:density + 
      chlorides:free.sulfur.dioxide:density:pH + chlorides:free.sulfur.dioxide:density:sulphates + 
      chlorides:free.sulfur.dioxide:density:alcohol + chlorides:free.sulfur.dioxide:pH:sulphates + 
      chlorides:free.sulfur.dioxide:pH:alcohol + chlorides:free.sulfur.dioxide:sulphates:alcohol + 
      chlorides:total.sulfur.dioxide:density:pH + chlorides:total.sulfur.dioxide:pH:sulphates + 
      chlorides:total.sulfur.dioxide:pH:alcohol + chlorides:density:sulphates:alcohol + 
      total.sulfur.dioxide:density:pH:alcohol + volatile.acidity:chlorides:sulphates:alcohol + 
      fixed.acidity:residual.sugar:chlorides:pH + volatile.acidity:citric.acid:free.sulfur.dioxide:alcohol + 
      volatile.acidity:pH:sulphates:alcohol + residual.sugar:chlorides:density:pH, data = WineQuality)

multigraphsummary(model4)

print(summary(model4))
cat("Model 3 AIC =", AIC(model4, k=2), "\n")

cat("Model 1 AIC =", AIC(model1, k=2), "\n")
cat("Model 2 AIC =", AIC(model2, k=2), "\n")
cat("Model 3 AIC =", AIC(model3, k=2), "\n")
cat("Model 4 AIC =", AIC(model4, k=2), "\n")

# stepwise search for best regression model
library(MASS)
fit <- lm(quality~.^4, data=WineQuality)
n = length(WineQuality$quality)
step <- stepAIC(fit, direction="both", k=2)
step$anova # display results

