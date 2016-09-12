lrGradFunction <- function(theta, X, y,lambda){

  #lrGradFunction Computes theta, regularised gradient for logistic regression
  #Initialize some useful values
  m = length(y) #number of training examples
  
  grad = rep(0,length(theta))
  
  temp <- theta
  temp[1] <- 0 
  
  X = as.matrix(X) #Converting X into a matrix for matrix operations
  
  grad = (1/m)*(t(X)%*%(sigmoid(X%*%theta)-y)) +  (lambda/(m))*(temp)
  
  
  grad
  
}