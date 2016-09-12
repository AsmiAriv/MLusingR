computeCostMulti = function (X, y, theta) {
  
#COMPUTECOSTMULTI Compute cost for linear regression with multiple variables
#COMPUTECOSTMULTI(X, y, theta) computes the cost of using theta as the
#parameter for linear regression to fit the data points in X and y
  
#Initialize some useful values
m = length(y) # number of training examples

# Return the following variables correctly 
J = 0

#Compute the cost of a particular choice of theta and set J to the cost.

J = (1/(2*m))*(t(X%*%theta - y))%*%(X%*%theta - y)
                 
return(J) 

}