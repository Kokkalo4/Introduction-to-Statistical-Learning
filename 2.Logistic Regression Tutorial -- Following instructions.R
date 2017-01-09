require(ISLR)
names(Smarket)
summary(Smarket)
?Smarket
pairs(Smarket , col = Smarket$Direction) #vgazei to plot pou ta exei ola paketo se ena, kai deixnei pws einai correlated.
#Sto sigekrimeno pairs to valame na kanei pairplot ta data kai na ta xrwmatisei me to Direction pou exei 2 values
#Up / Down


#Logistic Regression. family = binomial leei stin R na kanei logistic regression.
glm.fit <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume , 
               data = Smarket, family = binomial)

summary(glm.fit) #apo to summary fainetai oti kanena apo ta coefficients den einai significant. That doesnt mean that the model
#wont be able to make any kind of reasonable predictions, it just means that possibly these variables are very correlated.
#ALLA emeis kratame oti apla kanena variable DEN EINAI SIGNIFICANT.


glm.probs <- predict(glm.fit, type = "response") #From the model above we can make predictions. This will make predictions on the
#training data that we have used to fit the model. 
glm.probs[1:5] #And it gives you a vector of fitted probabilities. HERE WE CAN SEE THE FIRST 5, and we see that they're very
#close to 50%.

glm.pred <- ifelse(glm.probs > 0.5, "Up","Down") #we can turn the predictions above into classifications by thresholding at
#0.5. and we do it by using ifelse command.

#NEXT we are going to see the above models performance.
attach(Smarket) #first we attach the data.
table(glm.pred, Direction) #and then we make a table of glm.pred, which is our ups and downs from our prediction against the 
#true direction. 

mean(glm.pred == Direction) #mean classification performance. That's where glm.pred is equal to the direction, and we just
#take the mean of those. and this gives us a proportion which in this case is 0.52

#Because we may have overfit on the train data we 
#Split the data on train and test.
train <- Year<2005 #We create a logical , where if Year is <2005 we get a TRUE, else we get a FALSE.

#and then again we fit the model using our training data.
glm.fit <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume , 
               data = Smarket, family = binomial, subset = train)

#Then in the case of predict we use the data that is not flagged as TRAIN, or else test set. In this case data where 
#Year > 2005. 
glm.probs <- predict(glm.fit, newdata = Smarket[!train,], type = "response")
glm.pred <- ifelse(glm.probs > 0.5 , "Up","Down")


#Again we make a subset of the data. All the data involved here are test data.
Direction.2005 <- Smarket$Direction[!train]
#the first table is on test data.
table(glm.pred,Direction.2005)
#and now that we get the mean. we see that it is 0.48 which is worse than before. So we're doing WORSE than the NULL RATE, 
#which is 50%.
mean(glm.pred == Direction.2005)


#So what are going to do on this case. 
#We might be overfitting. 
#So what we do. 
#We're going to fit a smaller model. In this case we only use Lag1 and Lag2 and leave out all other variables.
glm.fit <- glm(Direction ~ Lag1 + Lag2, 
               data = Smarket, family = binomial, subset = train)

glm.probs <- predict(glm.fit, newdata = Smarket[!train,], type = "response")
glm.pred <- ifelse(glm.probs > 0.5 , "Up","Down")

table(glm.pred,Direction.2005)
mean(glm.pred == Direction.2005) #in this case we case we get an error rate of 55%. so we have a correct classification rate
#of about 56%.
#So using a smaller model appears to have done better in our case.
summary(glm.fit)
