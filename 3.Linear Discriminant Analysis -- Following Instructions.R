require(ISLR)
require(MASS)

#Linear Discriminant Analysis
lda.fit <- lda(Direction ~ Lag1 + Lag2, data = Smarket, subset = Year < 2005) #to do Linear Discriminant Analysis we use
#the lda function. Again we use the stock market data and we want to predict the direction for a particular day. We also
#use a subset of data, in this case where Year is <2005. Also, this will be used as train data.
lda.fit #REMEMBER , the LDA function is used to separate 2 groups. and so when we print out the summary, we get the 
#Coefficients of 2 separate groups.

plot(lda.fit) #It plots the linear discriminant function , separately the values of the linear discriminant function, separately
#for the UP group and for the DOWN group.

#Next we see how well our model performs. First we make a subset of the data, in this case we make subset Smarket.2005 and 
#the data in it are the data from the stock market of Year = 2005
Smarket.2005 <- subset(Smarket, Year = 2005)
#Next we call the predict method for the lda.fit. and we give it the fit we created above and the new data which we created from
#the above subset.
lda.pred <- predict(lda.fit , Smarket.2005)
lda.pred[1:5,]
class(lda.pred) #shows the class of the function we created.
data.frame(lda.pred)[1:5,] #since lda.pred[1:5,] wont print any data because it is a list we use data.frame(lda.pred)[1:5,], 
#which creates an output of data frame and we can see the requested data. From the printed output we see that it gives us 
#the row name, the classification, then its 2 columns , posterior.Down and posterior.Up and the actual value of LDA score.

table(lda.pred$class, Smarket.2005$Direction) #then we make the confusion matrix of the data. which consists of lda.pred$class
#(which is the predicted class) versus Smarket.2005$Direction(which is the TRUE value).

mean(lda.pred$class == Smarket.2005$Direction) #This tells us the current classification rate, which in this case is 52%.
