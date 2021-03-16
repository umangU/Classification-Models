# ISLR library for the stock market data and class library for the "knn" function
library(ISLR)
library(class)
attach(Smarket)
# Partitioning the dataset into training and testing
train = (Year<2005)
Smarket.2005 = Smarket[!train,]
dim(Smarket.2005)
Direction.2005=Direction[!train]
# Column binding the predictors
# train.X - matrix containing predictors associated with the training data
# test.X - matrix containing predictors associated with the testing data
# train.Direction - vector containing class labels for the training observations
train.X = cbind(Lag1,Lag2)[train,]
test.X = cbind(Lag1,Lag2)[!train,]
train.Directions = Direction[train]
# Setting seed to ensure reproducibility of results
set.seed(1)
# Fitting and making predictions using the KNN model with K=1
knn.pred = knn(train.X,test.X, train.Directions, k=1)
table(knn.pred, Direction.2005)
# Fitting and making predictions using the KNN model with K=3
knn.pred = knn(train.X,test.X, train.Directions, k=3)
table(knn.pred, Direction.2005)
# 53.6% correct predictions. No further improvement seen past K=3
