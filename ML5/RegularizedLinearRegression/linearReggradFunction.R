linearReggradFunction <- function(X, y, theta, lambda){
  
  #linearReggradFunction Performs gradient descent to calculate gradients
  #for regularized linear regression
  
  #Initialize some useful values
  m = length(y) # number of training examples

   X = as.matrix(X) #Converting to matrix for matrix operations
  theta <- as.matrix(theta)
  grad = rep(0,length(theta))
  
  temp = theta 
  temp[1] = 0
  
  grad = (1/m)*(t(X)%*%((X%*%theta - y))) + (lambda/(m))*(temp)
  
  grad
}