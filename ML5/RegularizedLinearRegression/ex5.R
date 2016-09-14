#This file uses the following functions

# To run this script please ensure to store these functions in 
# the same folder as this file and set this folder as working directory

#     featureNormalize.R (For narmalizing variables)
#     polyFeatures.R (For creating polynomial features)
#     learningCurve.R (For generating train and CV errors on each example)
#     trainLinearReg.R (for training the model)
#     validationCurve.R (For generating train and CV errors on different lambda values)
#     plotFit.R (For plotting fits)
#     linearRegCostFunction.R (For using optimizer in R to optimize weights at minimized cost)
#     linearReggradFunction.R (For using optimizer in R to optimize weights at minimized cost)


#Setting the working directory
setwd("C:/R/MachineLearning/ML5/RegularizedLinearRegression")

#Initializing
rm(list=ls()) #Using this code will remove all the objects from R session


#Loading featureNormalize function
source('featureNormalize.R')

#Loading polyFeatures function
source('polyFeatures.R')

#Loading learningCurve function
source('learningCurve.R')

#Loading trainLinearReg function
source('trainLinearReg.R')

#Loading validationCurve function
source('validationCurve.R')

#Loading plotFit function
source('plotFit.R')

#Loading linearRegCostFunction function
source('linearRegCostFunction.R')

#Loading linearReggradFunction function
source('linearReggradFunction.R')


library(R.matlab) #Required for reading .mat file

# Load Training Data
data <- readMat('ex5data1.mat') # training data stored in arrays X, y

#Train set
X <- data[[1]]
y <- data[[2]]

#Test set
Xtest <- data[[3]]
ytest <- data[[4]]

#Validation set
Xval <- data[[5]]
yval <- data[[6]]

dim(X)
dim(y)

m <- nrow(X)


#Plotting the data
plot(X, y,col="red", lwd=1.5, pch=3, xlab = "Change in water level (x)", 
     ylab = 'Water flowing out of the dam (y)')

#Regularized Linear Regression Cost
theta <- c(1,1)

X1 <- cbind(rep(1,m),X)
J = linearRegCostFunction(X1, y, theta, 1)

cat('Cost at theta = c(1 , 1):',
         '\n(this value should be about 303.993192)\n', J)


#Regularized Linear Regression gradiant

grad = linearReggradFunction(X1, y, theta, 1)

cat('Gradient at theta = c(1 , 1):',
    '\n(this value should be about [-15.303016; 598.250744])\n', grad)

#Train Linear Regression

#Train linear regression with lambda = 0
lambda=0

res <- trainLinearReg(X1, y, lambda)

theta <- res$theta
cost <- res$cost

plot(X, y,col="red", lwd=1.5, pch=3, xlab = "Change in water level (x)", 
     ylab = 'Water flowing out of the dam (y)')
lines(X, X1%*%theta, col="blue", lwd=2)


#Learning Curve for Linear Regression
lambda = 0

m1 <- nrow(Xval)
Xval1 <- cbind(rep(1,m1),Xval)

lc <- learningCurve(X1, y,Xval1, yval, lambda)
error_train <- lc$error_train
error_val <- lc$error_val

plot(1:(m-1), error_train,ylim = c(0,150), xlim=c(0,12),
     xlab ='Number of training examples', ylab='Errors', type = 'l',col="blue")
lines(1:(m-1), error_val, col="green")
legend("topright",legend=c('Training', 'Cross Validation'), lwd = 1, lty=c(1,1),col=c("blue","green"))

df <- data.frame(error_train, error_val,row.names = NULL)
df

#Feature Mapping for Polynomial Regression
p = 8

#Map X onto Polynomial Features and Normalize
X_poly = polyFeatures(X, p)

fn <- featureNormalize(X_poly)

X_poly <- fn$X

X_poly <- cbind(rep(1,nrow(X_poly)),X_poly) #Adding bias term

mu <- fn$mu

sigma <- fn$sigma

#Map X_poly_test and normalize (using mu and sigma)
X_poly_test = polyFeatures(Xtest, p)

for(i in 1:p){
X_poly_test[,i] = X_poly_test[,i]-mu[i]
X_poly_test[,i] = X_poly_test[,i]/sigma[i]
}

X_poly_test <- cbind(rep(1,nrow(X_poly_test)),X_poly_test) #Adding bias term

#Map X_poly_val and normalize (using mu and sigma)

X_poly_val = polyFeatures(Xval, p)

for(i in 1:p){
  X_poly_val[,i] = X_poly_val[,i]-mu[i]
  X_poly_val[,i] = X_poly_val[,i]/sigma[i]
}

X_poly_val <- cbind(rep(1,nrow(X_poly_val)),X_poly_val) #Adding bias term


cat('Normalized Training Example 1:\n')
X_poly[1,]

#Learning Curve for Polynomial Regression

lambda = 0
theta = trainLinearReg(X_poly, y, lambda)
theta <- theta$theta

plot(X, y, xlim=c(-80,80),ylim=c(-60,80),col='red', lwd = 1.5,
     xlab = "Change in water level (x)", 
     ylab = 'Water flowing out of the dam (y)',
     main=paste0('Polynomial Regression Fit ','(lambda = ',lambda,')'))
plotFit(min(X), max(X), mu, sigma, theta, p)


lc <- learningCurve(X_poly, y,X_poly_val, yval, lambda)
error_train <- lc$error_train
error_val <- lc$error_val

plot(1:(m-1), error_train,ylim = c(0,150), xlim=c(0,12),
     xlab ='Number of training examples', ylab='Errors', type = 'l',col="blue")
lines(1:(m-1), error_val, col="green")
legend("topright",legend=c('Training', 'Cross Validation'), lwd = 1, lty=c(1,1),col=c("blue","green"))

df <- data.frame(error_train, error_val,row.names = NULL)
df

# Validation for Selecting Lambda

vald <- validationCurve(X_poly, y, X_poly_val, yval)

lambda_vec <- vald$lambda_vec
error_train <- vald$error_train
error_val <- vald$error_val

plot(lambda_vec, error_train, xlab='lambda', ylab='Error', type = 'l',col="blue")
lines(lambda_vec, error_val, col="green")
legend("topright",legend=c('Training', 'Cross Validation'), lwd = 1, lty=c(1,1),col=c("blue","green"))

df <- data.frame(lambda_vec, error_train, error_val)
df

