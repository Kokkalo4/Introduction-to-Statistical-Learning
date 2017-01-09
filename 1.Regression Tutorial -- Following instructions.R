library(MASS)
library(ISLR)
#Simple Linear Regression.
names(Boston)
?Boston
plot(medv~lstat, Boston)
fit1 <- lm(medv ~ lstat, data = Boston)
fit1
#printing fit gives you a brief summary
summary(fit1)
#for more detailed summary call summary() command
#when calling summary we see the call of the model(command to create model) , coefficients -- we are not interested in 
#intercept that much as we are interested in lstat value(in this case).
abline(fit1, col = "red") #abline puts a line tha corresponds to the fit.
#We can also see additional components with the following commands.
names(fit1)
confint(fit1) #finds the confidence intervals for the fit.

#predict function is another one of the methods we can use to query a linear model fit. In this case we are going to predict
#with three new values for lstat( lstat = c(5,10,15)) which are 5,10,15 and in this case we do not ask only for predictions 
#but also for the confidence interval -- which is additional arguement to predict.
#When we call that function we get the fit at those three values(5,10,15) and then the lower and upper confidence band.
#Finally, we can give as many values as we like and get those confidence intervals.
predict(fit1, data.frame(lstat = c(5,10,15)), interval = "confidence")

#Multiple Linear Regression.
fit2 <- lm(medv ~ lstat + age, data = Boston)
#From summary we check p-value for significance and R-Squared(in this case Multiple R-Squared),
#REMEMBER IMPORTANT: The higher the R-Squared the better.
summary(fit2)

#fit3 uses all variables except for medv which is our response variable.
fit3 <- lm(medv ~ . , data = Boston)
#from summary of fit3 and by looking at p-values we see that most of the variables are significant but on this case where
#we put on all variables age now is not significant as it was when was paired only with lstat variable. This means that 
#there are many other predictors correlated with age and age is no longer required.
summary(fit3)
#plot linear models. First we make a 2 by 2 matrix to fit the graphical output which will be 4 graphs.
par(mfrow = c(2,2))
plot(fit3)
#Plots give you various views of the model. The first one(upper left) is the residuals against the fitted values -- the reason
#we make this plot is because we look for non-linearities. and from this particular plot we see that there is non-linearity
#as the model is not capturing everything that's going on.
#all other plots will give you aspects of the linear model fit.


#use update command to update a model already existing.
#in this case we put fit3 and update it by removing age and indus variables.
fit4 <- update(fit3, ~. -age-indus)
summary(fit4)


#Non-Linear Terms and Interactions.

#fit5 we make a fit where we put on an interactions between lstat and age.
# * means(in formula language) we have an interactions between the variables
fit5 <- lm(medv ~ lstat*age, data = Boston)
summary(fit5)
#from fit5 summary we see that although age seems to not be significant the interaction between lstat and age(lstat:age) seems
#be somewhat significant.

#In fit6 since we saw before that there was a non-linear looking scatter plot between medv and lstat
#we explicitelly put on a quadratic term.
#The quadratic term is indicated by "lstat^2" but we protect it with Identity function and so the output is I(lstat^2)
fit6 <- lm(medv ~ lstat + I(lstat^2), data = Boston) 
#from summary we see that both lstat and the quadratic coefficients are significant.
summary(fit6)

#plotting fit6
#attach means the named variables of data Boston are available in our workplace.
attach(Boston)
par(mfrow = c(1,1))
plot(medv~lstat)
#we cant use abline anymore because abline is only for linear relations, so we have to fit another line. We do it by points()
#command as written below.
points(lstat, fitted(fit6), col = "red", pch = 20)


#easy way to putting polynomials.
#in this case we fit medv as a polynomial of degree four in lstat.
fit7 <- lm(medv ~ poly(lstat,4))
points(lstat, fitted(fit7), col = "blue", pch = 20 )

#see what plotting characters are available.
plot(1:20,1:20,pch = 1:20, cex =2)

#Qualitative predictors.

#Load data Carseats.+
library(ISLR)
data(Carseats)
#get variable names of dataset.
names(Carseats)
#Also do a summary.
summary(Carseats)


#fit a model with qualitative values on it.
fit1 <- lm(Sales ~ . + Income:Advertising + Age:Price, data = Carseats )
#again we see some significant variables and also some unsignificant. Also from summary we see and the interactions that we 
#have put in. Income:Advertising seem to be significant while Price:Age seems to be not so significant.
summary(fit1)
#Contrasts functions shows you how R will code that variable when it's put in a linear model. In this case its a 3 level factor
#and so it puts on two dummy variables and the values are if ShelveLoc is Bad is zero for Both , if its Good is 1 for Good and 
#0 for Medium and if Medium is 0 for Good and 1 for Medium.
contrasts(Carseats$ShelveLoc)




#Writing R functions to fit a regression model and make a plot.
regplot <- function(x,y) {
  fit <- lm(x ~ y)
  plot(x,y)
  abline(fit, col = "red")
}
attach(Carseats)
regplot(Price,Sales)

#Same function with dots. Dots allow you to add extra arguements in function.
regplot <- function(x,y,...) {
  fit <- lm(x ~ y)
  plot(x,y,...)
  abline(fit, col = "red")
}
regplot(Price,Sales, xlab = "Price" , ylab = "Sales", col = "blue", pch = 20)
