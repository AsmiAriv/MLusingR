gradThetaNewton <- function(theta, X, y,num_iter){
  
#gradThetaNewton Computes theta, cost and gradient for logistic regression
#using hessian for optimization in order to minimize the cost w.r.t. to the parameters.

# Packages required
library(numDeriv)
library(MASS)
  
#Initialize some useful values
m = length(y) #number of training examples

J_history = rep(0, num_iter)

grad = rep(0,length(theta))

X = as.matrix(X) #Converting X into a matrix for matrix operations

 for (iter in 1:num_iter) {

grad = (1/m)*(t(X)%*%(sigmoid(X%*%theta)-y))

#hes = hessian(costFunction, theta, method = "complex", X = X, y = y)

#hes = (1/m)*t(X)%*%((sigmoid(X%*%theta))%*%t(1-sigmoid(X%*%theta)))%*%X #This does converge for small or large number of variables, but doesn't perform well

hes = (1/m)*t(X)%*%X*diag(sigmoid(X%*%theta))*diag(1-sigmoid(X%*%theta)) #This converges well, but for large number of variables it diverges
theta = theta - ginv(hes)%*%grad 
    
J_history[iter] <- costFunction(theta,X, y)
  }

  
list(theta=theta, gradient=grad,J_history=J_history)
  
  
}