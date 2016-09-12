gradientDescentMulti <- function(X, y, theta, alpha, num_iters){
  
#GRADIENTDESCENTMULTI Performs gradient descent to learn theta
#theta = GRADIENTDESENTMULTI(X, y, theta, alpha, num_iters) updates theta by 
#taking num_iters gradient steps with learning rate alpha
  
#Initialize some useful values
  m = length(y) # number of training examples
  J_history = rep(0, num_iters)
  
  for (iter in 1:num_iters) {
    
    delta = (1/m)*(t(X)%*%((X%*%theta - y)))
    theta = theta - alpha*delta

    J_history[iter] <- computeCostMulti(X, y, theta)
  }
  list(theta=theta, J_history=J_history)
}