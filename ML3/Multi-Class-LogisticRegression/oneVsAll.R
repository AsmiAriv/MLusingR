oneVsAll <- function(X, y, num_labels, lambda){
  
#ONEVSALL trains multiple logistic regression classifiers and returns all
#the classifiers in a matrix all_theta, where the i-th row of all_theta 
#corresponds to the classifier for label i
#   [all_theta] = ONEVSALL(X, y, num_labels, lambda) trains num_labels
#   logisitc regression classifiers and returns each of these classifiers
#   in a matrix all_theta, where the i-th row of all_theta corresponds 
#   to the classifier for label i
  
# Some useful variables
  
  m <- nrow(X)
  n <- ncol(X)
  
  all_theta <- matrix(rep(0, num_labels*( n + 1)), num_labels)
  
  #Adding a column for intercept to the predictors X
  X <- cbind(rep(1,m),X)
  X <- as.matrix(X)
  
  X = as.matrix(X) #Converting X into a matrix for matrix operations
  
  for(i in 1:num_labels) {
  
  initial_theta <- rep(0,n+1)
  c = i
  
  costh <- optim(par=initial_theta, fn=lrCostFunction, 
                 gr=lrGradFunction, method="BFGS", X=X,y=as.integer(y==c),lambda=lambda)
  
  theta <- costh$par
  
  all_theta[i,] = theta
  
  }
  
 all_theta 
  
  
}