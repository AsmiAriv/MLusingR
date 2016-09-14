linearRegCostFunction = function (X, y, theta,lambda) {
  
  #linearRegCostFunction Compute cost for regularized linear regression with multiple variables
  #linearRegCostFunction(X, y, theta,lambda) computes the cost of using theta as the
  #parameter for linear regression to fit the data points in X and y
  
  #Initialize some useful values
  m = length(y) # number of training examples
	
  X = as.matrix(X) #Converting to matrix for matrix operations
  
  # Return the following variables correctly 
  J = 0
  
  #Compute the cost of a particular choice of theta and set J to the cost.
  
  temp = theta 
  temp[1] = 0
  
  J = (1/(2*m))*(t(X%*%theta - y))%*%(X%*%theta - y) + (lambda/(2*m))*t(temp)%*%(temp)
  
  return(J) 
  
}