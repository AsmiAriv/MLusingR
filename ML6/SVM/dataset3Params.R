dataset3Params <- function(X, y, Xval, yval){
  
  #EX6PARAMS returns your choice of C and sigma for Part 3 of the exercise
  #where you select the optimal (C, sigma) learning parameters to use for SVM
  #with RBF kernel
  #   [C, sigma] = EX6PARAMS(X, y, Xval, yval) returns your choice of C and 
  #   sigma. You should complete this function to return the optimal C and 
  #   sigma based on a cross-validation set.
  
  
  C = 1
  sigma = 0.3
  l=0
  
  vec = c(0.001, 0.01, 0.03, 0.1, 0.3, 1, 3, 10)
  
  error_val = matrix(rep(0, 3*((length(vec))^2)),ncol=3)
  
  for(i in 1:length(vec)){
    C = vec[i]
    
    for(j in 1:length(vec)){
      sigma = vec[j]
      error_val[l+j,1] = C
      error_val[l+j,2] = sigma
      model= svmTrain(X, y, C, sigma,gaussianKernel)
      predictions = svmPredict(model, Xval)
      error_val[l+j,3] =  mean(as.numeric(predictions!= as.vector(yval)))
    }
    l = l+ (length(vec))
  }
  
  miner = error_val[order(error_val[,3]),]
  
  C = miner[1,1]
  sigma = miner[1,2]
  
  list(C=C,sigma=sigma)
  
}