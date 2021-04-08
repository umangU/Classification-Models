# ISLR library for the stock market data and Mass library for the "qda" function
library(MASS)
library(ISLR)
attach(Smarket)
# Reading additional dataset
dummy = read_xlsx(file.choose())
# Partitioning the dataset into training and testing
train = (Year<2005)
Smarket.2005 = Smarket[!train,]
dim(Smarket.2005)
Direction.2005=Direction[!train]
# Fitting the QDA model using the predictors with least p-values
qda.fit = qda(Direction~Lag1+Lag2, data=Smarket, subset=train)
qda.fit
# Predicting on the test dataset
qda.class = predict(qda.fit, Smarket.2005)$class
table(qda.class, Direction.2005)
mean(qda.class==Direction.2005)
