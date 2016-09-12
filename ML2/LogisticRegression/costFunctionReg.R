costFunctionReg <- function(theta, X, y,lambda){
  
#COSTFUNCTIONREG Compute cost and gradient for logistic regression with regularization
#J = COSTFUNCTIONREG(theta, X, y, lambda) computes the cost of using
#theta as the parameter for regularized logistic regression and the

  
#Initialize some useful values

m = length(y) #number of training examples
  
J = 0
  
X = as.matrix(X) #Converting X into a matrix for matrix operations

temp <- theta
temp[1] <- 0  
  
J = (1/m)*(-t(y)%*%log(sigmoid(X%*%theta))-(t(1-y))%*%log(1-sigmoid(X%*%theta))) +
 (lambda/(2*m))*(t(temp))%*%(temp)

J
  
}