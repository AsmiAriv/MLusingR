#This file uses the following functions

# To run this script please ensure to store these functions in 
# the same folder as this file and set this folder as working directory

#     sigmoid.R
#     gradThetaNewtonReg.R
#     predict.R
#     costFunctionReg.R
#     plotData.R
#     mapFeature.R
#     plotDecisionBoundary.R


#Setting the working directory
setwd("C:/R/MachineLearning/ML2/LogisticRegression")

# loading data

data <- read.table('ex2data2.txt',sep=",")

X <- data[,1:2]
y <- data[,3]
m = length(y)

#Loading gradThetaNewtonReg function
source('gradThetaNewtonReg.R')

#Loading regularized gradient function
source('gradReg.R')

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

#Add Polynomial Features

#Note that mapFeature also adds a column of ones for us, so the intercept
#term is handled
X = mapFeature(X[,1], X[,2])

#Initializing the fitting parameters
initial_theta <- rep(0,dim(X)[2])

# Set regularization parameter lambda to 1 (you should vary this)
lambda = 1

#Compute and display theta and cost using optim() as optimizer
#num_iter = 400

costh <- optim(par=initial_theta, fn=costFunctionReg, gr=gradReg, method="BFGS", X=X,y=y,lambda=lambda)


theta <- costh$par
#grad <- costh$gradient
cost <- costh$value


cat('Cost at theta found after optimization:', '\n', cost)
cat('theta found after optimization:', '\n',theta)
#cat('Gradient found after optimization:', '\n',grad)

#Plot Boundary
plotDecisionBoundary(theta, X, y)

#Predict probability for a student with score 45 on exam 1
#and score 85 on exam 2 

p = predict(theta, X)

cat('Train Accuracy:\n',mean((p == y))*100)


#Compute and display theta and cost using nlminb() as optimizer
#num_iter = 400

costh <- nlminb(start=initial_theta, objective=costFunctionReg, gradient=gradReg, X=X,y=y,lambda=lambda, control = list(iter.max=400))


theta <- costh$par
#grad <- costh$gradient
cost <- costh$objective


cat('Cost at theta found after optimization:', '\n', cost)
cat('theta found after optimization:', '\n',theta)
#cat('Gradient found after optimization:', '\n',grad)

p = predict(theta, X)

cat('Train Accuracy:\n',mean((p == y))*100)


#Compute and display theta and cost using gradThetaNewtonReg function as optimizer
num_iter = 100

costh <- gradThetaNewtonReg(initial_theta, X, y,num_iter,lambda)


theta <- costh$theta
grad <- costh$gradient
cost <- costh$J_history
hes <- costh$h

cat('Cost at theta found after optimization:', '\n', cost[num_iter])
cat('theta found after optimization:', '\n',theta)
cat('Gradient found after optimization:', '\n',grad)

p = predict(theta, X)

cat('Train Accuracy:\n',mean((p == y))*100)
plot(1:num_iter,cost)

