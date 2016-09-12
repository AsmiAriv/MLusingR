#This file uses the following functions

# To run this script please ensure to store these functions in 
# the same folder as this file and set this folder as working directory

#     sigmoid.R
#     costFunction.R
#     gradThetaNewton.R
#     predict.R
#     costFunctionReg.R
#     plotData.R
#     mapFeature.R
#     plotDecisionBoundary.R


#Setting the working directory
setwd("C:/R/MachineLearning/ML2/LogisticRegression")

# loading data

data <- read.table('ex2data1.txt',sep=",")

X <- data[,1:2]
y <- data[,3]
m = length(y)

#Loading cost function
source('costFunction.R')

#Loading gradThetaNewton function
source('gradThetaNewton.R')

#Loading cost function with regularization
source('costFunctionReg.R')

#Loading predict function
source('predict.R')

#Loading sigmoid function
source('sigmoid.R')

#Loading mapFeature function
source('mapFeature.R')

#Loading plotData function
source('plotData.R')

#Loading plotDecisionBoundary function
source('plotDecisionBoundary.R')

#Plotting data
plotData(X, y)

#Adding a column for intercept to the predictors X
X <- cbind(rep(1,m),X)
X <- as.matrix(X)

#Initializing the fitting parameters
initial_theta <- rep(0,dim(X)[2])

#Compute and display theta, cost and gradient after optimizatio
#using gradThetaNewton function

num_iter = 400

costh <- gradThetaNewton (initial_theta, X,y,num_iter)

theta <- costh$theta
grad <- costh$gradient
cost <- costh$J_history[num_iter]

cat('Cost at theta found after optimization:', '\n', cost)
cat('theta found after optimization:', '\n',theta)
cat('Gradient found after optimization:', '\n',grad)

#Plot Boundary
plotDecisionBoundary(theta, X, y)

#Predict probability for a student with score 45 on exam 1
#and score 85 on exam 2 

prob = sigmoid(c(1, 45, 85)%*%theta)

cat('For a student with scores 45 and 85, we predict an admission probability of \n',prob)

p = predict(theta, X)

cat('Train Accuracy:\n',mean((p == y))*100)

