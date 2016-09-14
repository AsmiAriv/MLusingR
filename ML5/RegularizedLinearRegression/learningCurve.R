learningCurve <- function(X, y, Xval, yval, lambda){
  
  
  #LEARNINGCURVE Generates the train and cross validation set errors needed 
  #to plot a learning curve
  #   [error_train, error_val] = ...
  #       LEARNINGCURVE(X, y, Xval, yval, lambda) returns the train and
  #       cross validation set errors for a learning curve. In particular, 
  #       it returns two vectors of the same length - error_train and 
  #       error_val. Then, error_train[i] contains the training error for
  #       i examples (and similarly for error_val[i]).
  #
  #   In this function, you will compute the train and test errors for
  #   dataset sizes from 1 up to m. In practice, when working with larger
  #   datasets, you might want to do this in larger intervals.
  
  
  # Number of training examples
  m = length(y)
  
  error_train = rep(0,m-1)
  error_val   = rep(0,m-1)
  
  for(i in 2:m){
    
    # Compute train/cross validation errors using training examples 
    # X[1:i,] and y[1:i], storing the result in 
    # error_train[i] and error_val[i]
     
    theta = trainLinearReg(X[1:i,], y[1:i], lambda);
    theta = theta$theta
    error_train[i-1] = linearRegCostFunction((X[1:i,]), y[1:i], theta, 0);
    error_val[i-1] =  linearRegCostFunction(Xval, yval, theta, 0);
    
   }
  
  list(error_train=error_train,error_val=error_val)
}