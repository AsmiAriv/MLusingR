learningCurveRandomized <- function(X, y, Xval, yval, lambda,n){
  
  
  #learningCurveRandomized Generates the train and cross validation set errors needed 
  #to plot a learning curve based on random i observations
  #   [error_train, error_val] = ...
  #       learningCurveRandomized(X, y, Xval, yval, lambda) returns the train and
  #       cross validation set errors for a learning curve. In particular, 
  #       it returns two values - error_train and 
  #       error_val averaged over various random i observations.
  #
  #   In this function, you will compute the train and test errors for
  #   dataset sizes from 1 up to m and take the average of all the errors. In practice, when working with larger
  #   datasets, you might want to do this in larger intervals.
  
  
 # Number of training/cross validation examples
if(length(y)<=length(yval)) m = length(y)
else m = length(yval)
  
  error_train = rep(0,m-1)
  error_val   = rep(0,m-1)

  er_tr = rep(0,n)
  er_vl = rep(0,n)
  
  for(i in 2:m){
    # Compute train/cross validation errors using training examples 
    # X[ran,] and y[ran], storing the result in 
    # error_train[i] and error_val[i]
    
    for(j in 1:n){ 
       ran <- sample(m,i)
       theta = trainLinearReg(X[ran,], y[ran], lambda);
       theta = theta$theta
       er_tr[j] = linearRegCostFunction((X[ran,]), y[ran], theta, 0);
       er_vl[j] =  linearRegCostFunction(Xval[ran,], yval[ran], theta, 0);
        }
   error_train[i-1] <- mean(er_tr)
   error_val[i-1] <- mean(er_vl)
  }
  
  list(error_train=error_train,error_val=error_val)
}