lrCostFunction <- function(theta, X, y, lambda){
  
#LRCOSTFUNCTION Compute cost for logistic regression with 
#regularization
#J = LRCOSTFUNCTION(theta, X, y, lambda) computes the cost using
#theta as the parameter for regularized logistic regression and the
#gradient of the cost w.r.t. to the parameters. 

#Initialize some useful values
m = length(y) #number of training examples

J = 0

temp <- theta
temp[1] <- 0  

J = (1/m)*(-t(y)%*%log(sigmoid(X%*%theta))-(t(1-y))%*%log(1-sigmoid(X%*%theta))) +
  (lambda/(2*m))*(t(temp))%*%(temp)

J

}
