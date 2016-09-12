computeCost <- function(X,y,theta) {
  
#computeCost compute costs for linear regression
#COMPUTECOST(X, y, theta) computes the cost of using theta as the
#parameter for linear regression to fit the data points in X and y

#Initialize some useful values
m <- length(y)

#Return the following variables correctly

J=0

#Compute the cost of a particular choice of theta and set J to the cost.

J = (1/(2*m))*sum((X%*%theta - y)^2);

return(J)   
  
}