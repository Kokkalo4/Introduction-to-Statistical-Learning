require(ISLR)
require(boot)
?cv.glm
plot(mpg ~ horsepower , data = Auto)


#LOOCV(LEAVE ONE OUT CROSS-VALIDATION ) -- We use the Auto data and more specifically the relation mpg ~ horsepower 
#to investigate cross-validation.
glm.fit <- glm(mpg ~ horsepower, data = Auto) #First we fit a linear model using glm() function to fit it. glm() fits linear 
#models as well apart from non-linear models such as logistic regression.

cv.glm(Auto, glm.fit)$delta #runs cross validation n-1 times so it may be slow. The output of this function is 2 numbers
#the first number is the raw leave-one-out or lieu cross validation result. The second one is the bias corrected version 
#of it. The bias correction has to do with the fact that the data that we train it on is slightly smaller than the one
#that we would actually like to get the error for, which is the full dataset of size n.

#Now we write our own function to compute what happened above. 
loocv <- function(fit){
  h <- lm.influence(fit)$h #lm.influence is a post processor for lm.fit which gives you and extracts the element h.
  mean(residuals((fit)/(1-h))^2))
  }

#Now we try the above formula
loocv(glm.fit)

cv.error <- rep(0,5)
