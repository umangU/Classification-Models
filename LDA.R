# ISLR library for the stock market data and Mass library for the "lda" function
library(ISLR)
library(MASS)
attach(Smarket)
# Reading additional dataset
dummy = read_xlsx(file.choose())
# Partitioning the dataset into training and testing
train = (Year<2005)
Smarket.2005 = Smarket[!train,]
dim(Smarket.2005)
Direction.2005=Direction[!train]
# Fitting the LDA model using the predictors with least p-values
lda.fit = lda(Direction~Lag1+Lag2, data=Smarket, susbet=train)
# Plot of linear discriminants for each of the training observations
plot(lda.fit)
# Predicting on the test dataset
lda.pred=predict(lda.fit, Smarket.2005)
lda.class = lda.pred$class
table(lda.class, Direction.2005)
# Caluclate the mean error
mean(lda.class==Direction.2005)
# Applying 50% threshold to the posterior probability to re-create the predictions
sum(lda.pred$posterior[,1]>=0.5)
sum(lda.pred$posterior[,1]<0.5)
