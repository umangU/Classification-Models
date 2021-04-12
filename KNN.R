# ISLR library for the stock market data and class library for the "knn" function
library(ISLR)
library(class)
attach(Smarket)
# Reading additional dataset
dummy = read_xlsx(file.choose())
# Partitioning the dataset into training and testing
train = (Year<2005)
Smarket.2005 = Smarket[!train,]
dim(Smarket.2005)
Direction.2005=Direction[!train]
# Column binding the predictors
# train.X - predictors associated with the training data (Matrix)
# test.X - predictors associated with the testing data (Matrix)
# train.Direction - class labels for the training observations (Vector)
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
# 53.6% correct predictions. 
# Thus, No further improvement is observed past K=3
