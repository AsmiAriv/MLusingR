validationCurve <- function(X, y, Xval, yval){
  
  #VALIDATIONCURVE Generate the train and validation errors needed to
  #plot a validation curve that we can use to select lambda
  #   [lambda_vec, error_train, error_val] = ...
  #       VALIDATIONCURVE(X, y, Xval, yval) returns the train
  #       and validation errors (in error_train, error_val)
  #       for different values of lambda. You are given the training set (X,
  #       y) and validation set (Xval, yval).
  
  
  # Selected values of lambda (you should not change this)
  lambda_vec = c(0, 0.001, 0.003, 0.01, 0.03, 0.1, 0.3, 1, 3, 10)
  
  m <- length(lambda_vec)

  theta_val <- matrix(rep(0,length(lambda_vec)*ncol(X)),ncol=ncol(X))
  
  error_train = rep(0,m)
  error_val   = rep(0,m)
  
  for(i in 1:m){
    
    lambda <- lambda_vec[i]
    res = trainLinearReg(X, y, lambda);
    theta_val[i,] = res$theta
    error_train[i] = linearRegCostFunction(X, y, theta_val[i,], 0);
    error_val[i] =  linearRegCostFunction(Xval, yval, theta_val[i,], 0);
    
  }
  
  
  list(lambda_vec=lambda_vec, error_train=error_train, error_val=error_val, theta=theta_val)  
}