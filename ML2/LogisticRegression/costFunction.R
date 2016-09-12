costFunction <- function(theta, X, y){
  
#COSTFUNCTION Compute cost and gradient for logistic regression
#J = COSTFUNCTION(theta, X, y) computes the cost of using theta as the
#parameter for logistic regression and the gradient of the cost
#w.r.t. to the parameters.
  
#Initialize some useful values
m = length(y) #number of training examples

J = 0

grad = rep(0,length(theta))

X = as.matrix(X) #Converting X into a matrix for matrix operations


J = (1/m)*(-t(y)%*%log(sigmoid(X%*%theta))-(t(1-y))%*%log(1-sigmoid(X%*%theta)))

J
  
  
}