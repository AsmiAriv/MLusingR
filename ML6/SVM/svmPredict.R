svmPredict <- function(model, X){
  
  #SVMPREDICT returns a vector of predictions using a trained SVM model
  #(svmTrain). 
  #   pred = SVMPREDICT(model, X) returns a vector of predictions using a 
  #   trained SVM model (svmTrain). X is a mxn matrix where there each 
  #   example is a row. model is a svm model returned from svmTrain.
  #   predictions pred is a m x 1 column of predictions of {0, 1} values.
  
  
  # Check if we are getting a column vector, if so, then assume that we only
  # need to do prediction for a single example
  X <- as.matrix(X)
  if (ncol(X) == 1) X <- t(X) #Examples should be in rows
  
  #Dataset
  m <- nrow(X)
  p <- rep(0,m)
  pred <- rep(0,m)
  
  #Retrieving model data
  model.kernelFunction <- model$model.kernelFunction
  model.X <- model$model.X
  model.y <- model$model.y
  model.alphas <- model$model.alphas
  model.b <- model$model.b
  model.w <- model$model.w
  sigma <- model$model.sigma
  
  if (identical(model.kernelFunction,linearKernel)) p = (X%*%model.w) + as.numeric(model.b)
  else if (identical(model.kernelFunction,gaussianKernel)){
    X1 <- matrix(rowSums(X^2))
    X2 <- t(matrix(rowSums(model.X^2)))
    K <- sweep(sweep(-2*X%*%t(model.X),2,X2,FUN="+"),1,X1,FUN="+")
    K <- matrix(rep(model.kernelFunction(1, 0,sigma),length(K)),nrow(K))^K
    K <- sweep(K,2,t(model.y),FUN="*")
    K <- sweep(K,2,t(model.alphas),FUN="*")
    p <- rowSums(K)
    
  }
  else{
    #For other non-linear kernel
    for(i in 1:m){
      prediction = 0
      for(j in 1:rnow(model.X)){
        prediction = prediction + 
          model.alphas[j]*model.y[j]*model.kernelFunction(t(X[i,]), t(model.X[j,]))
      }
      p[i] <- prediction + as.numeric(model.b) 
    }
  }
  # Convert predictions into 0 / 1
  pred[p >= 0] =  1
  pred[p <  0] =  0
  pred
}