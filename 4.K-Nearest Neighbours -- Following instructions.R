#K- Nearest Neighbours.
library(class)
?knn
attach(Smarket)
XLag <- cbind(Lag1,Lag2) #Create a matrix of Lag1 and Lag2
train <- Year < 2005 #make train data which consist of data that are Year < 2005.

#Then we call knn function. in this case we use 1 nearest neighbor classification.
knn.pred <- knn(XLag[train,], XLag[!train,], Direction[train], k = 1)
table(knn.pred,Direction[!train]) #We make a table of knn.pred which is the prediction and Direction[!train] which is the 
#true response which is Direction and not train. The output would be a confusion matrix.
mean(knn.pred == Direction[!train]) #Check classification performance, which is exactly 50%. Which tells us that 1 nearest neighbour
#classification is no better than flipping a coin.
