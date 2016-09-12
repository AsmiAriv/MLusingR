gradThetaNewtonReg <- function(theta, X, y,num_iter,lambda){
  
#gradThetaNewtonReg Computes theta, regularised cost (using costFunctionReg function)  and 
#regularised gradient for logistic regression
#using hessian for optimization in order to minimize the cost w.r.t. to the parameters.

#Required package
library(MASS)
library(numDeriv)

#Initialize some useful values
m = length(y) #number of training examples

J_history = rep(0, num_iter)

grad = rep(0,length(theta))

temp <- theta
temp[1] <- 0 

X = as.matrix(X) #Converting X into a matrix for matrix operations

 for (iter in 1:num_iter) {

grad = (1/m)*(t(X)%*%(sigmoid(X%*%theta)-y)) +  (lambda/(m))*(temp)

#hessian = (1/m)*t(X)%*%X*diag(sigmoid(X%*%theta))*diag(1-sigmoid(X%*%theta)) 

#hessian = (1/m)*t(X)%*%((sigmoid(X%*%theta))%*%t(1-sigmoid(X%*%theta)))%*%X +(lambda/(m))

hessian = hessian(costFunctionReg, theta, method = "complex", X = X, y = y,lambda=lambda)

theta = theta - ginv(hessian)%*%grad 
    
J_history[iter] <- costFunctionReg(theta,X, y,lambda)
  }
  
list(theta=theta, gradient=grad,J_history=J_history, h=hessian)
  
}