# ISLR library to access the stock market data
library(ISLR)
# Dataset dimesnion and variable names 
names(Smarket)
dim(Smarket)
# Pairwise correlations among the numerical predictors in the data-set
cor(Smarket[,-9])
attach(Smarket)
# Fitting the Logistic Regression model using all predictors and whole data-set
glm.fit = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data = Smarket, family=binomial)
summary(glm.fit)
# Accessing the fitted model's coefficients and it's p-values
coef(glm.fit)
summary(glm.fit)$coef[,4]
# Predicting the probability that market will go "Up"
glm.probs=predict(glm.fit,type="response")
glm.probs[1:10]
contrasts(Direction)
# Converting the predicted probability into predictions
glm.pred=rep("Down",1250)
glm.pred[glm.probs>0.5]="Up"
# Confusion matrix
table(glm.pred, Direction)
mean(glm.pred==Direction)
# 47.8%  error rate

############################################################################################################

# Partitioning the dataset into train and test
train = (Year<2005)
Smarket.2005 = Smarket[!train,]
dim(Smarket.2005)
Direction.2005=Direction[!train]
# Fitting the Logistic Regression model using the train dataset
glm.fit = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data = Smarket, family=binomial, subset=train)
# Getting predictions on the test dataset
glm.probs = predict(glm.fit,Smarket.2005,type="response")
glm.pred=rep("Down",252)
glm.pred[glm.probs>0.5]="Up"
mean(glm.pred == Direction.2005)
mean(glm.pred != Direction.2005)
# 52% test error rate (Worse than Random guessing!)

############################################################################################################

# Fitting the model using only the predictors with smallest p-values
glm1.fit = glm(Direction~Lag1+Lag2, data=Smarket, family=binomial, subset = train)
glm1.probs = predict(glm1.fit, Smarket.2005, type = "response")
glm1.pred = rep("Down", 252)
glm1.pred[glm1.probs>0.5] = "Up"
table(glm1.pred, Direction.2005)
mean(glm1.pred==Direction.2005)
mean(glm1.pred!=Direction.2005)
# Approx. 44% test error rate (better than random guessing approach though!)
# Predictions using new dummy data
predict(glm1.fit,newdata=data.frame(Lag1=c(1.2,1.5), Lag2=c(1.1,-0.8)), type="response")