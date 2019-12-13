
# Baseball-Data
#Major League Baseball Data from the 1986 and 1987 seasons.

library(MASS)
library(ISLR)

dim(Hitters )
names(Hitters)

# Exploratory data analysis
#summary statistics
attach(Hitters)
summary(Hitters)
str(Hitters)
#Check for missing values
sum(is.na(Hitters))
#removes all of the rows that have missing values in any variable
Hitters = na.omit(Hitters)
dim(Hitters ) 
sum(is.na(Hitters))

#Variable Selection
# Best subset selection

library (leaps)
regfit.full=regsubsets (Salary~.,Hitters)
summary (regfit.full) 
#outputs the best set of variables for each model size
regfit.full=regsubsets (Salary~.,data=Hitters ,nvmax=19) 
#???t up to a 19-variable model
reg.summary =summary (regfit.full) 
reg.summary

names(reg.summary) 

reg.summary$rsq 
#The R2 statistic increases as more variables are included

which.max(reg.summary$adjr2) 
#identify the location of the maximum point of a vector
plot(reg.summary$adjr2 ,xlab="Number of Variables ", ylab="Adjusted RSq",type="l")

points (11,reg.summary$adjr2[11], col="red",cex=2,pch=20)

which.min(reg.summary$cp ) 

plot(reg.summary$cp ,xlab="Number of Variables ",ylab="Cp", type="l") 

points (10,reg.summary$cp [10],col="red",cex=2,pch=20) 

which.min(reg.summary$bic )

plot(reg.summary$bic ,xlab="Number of Variables ",ylab="BIC", type="l")

points(6,reg.summary$bic [6],col="red",cex=2,pch =20)

# display the selected variables for the best model 
plot(regfit.full ,scale="adjr2")

plot(regfit.full ,scale="Cp")

plot(regfit.full ,scale="bic")


#to see the coeffcient estimates associated with the best model
coef(regfit.full ,6) 


#Fitting Linear-Regression 

#Split data set into 80:20 train and test data
set.seed(2)
index <- sample(nrow(Hitters), nrow(Hitters) * 0.80)
Hitters.train <- Hitters[index, ]
Hitters.test <- Hitters[-index, ]
model1 = lm( Salary ~ ., data = Hitters.train)
model1.sum = summary(model1)
model1.sum

#Remove the insignificant variable based on best subset selection
model2 = lm( Salary ~ AtBat+ Hits+Walks+CRBI+Division+PutOuts, data = Hitters.train)
model2.sum = summary(model2)
model2.sum

#Model selection 

#R-squared
model1.sum$r.squared

model2.sum$r.squared

#AIC
AIC(model1)

AIC(model2)

#BIC
BIC(model1)

BIC(model2)

#Test error (MSSE)
model1.pred.test <- predict(model1, newdata = Hitters.test)
model1.msse = mean((model1.pred.test - Hitters.test$Salary) ^ 2)
model1.msse

model2.pred.test <- predict(model2, newdata = Hitters.test)
model2.mspe <- mean((model2.pred.test - Hitters.test$Salary) ^ 2)
model2.mspe

#Model Assessment

par(mfrow = c(2,2))
plot(model2)
par(mfrow = c(1,1))

