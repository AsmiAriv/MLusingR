predict <- function(Theta1, Theta2, X){
  
  #PREDICT Predict the label of an input given a trained neural network
  #   p = PREDICT(Theta1, Theta2, X) outputs the predicted label of X given the
  #   trained weights of a neural network (Theta1, Theta2)
  
  # Useful values
  X = as.matrix(X) #Converting into matrix for matrix operations
  
  m = nrow(X)
  num_labels = length(Theta2)
  
  
  p = rep(0,m)
  X = cbind(rep(1,m), X)
   
  X = as.matrix(X) #Converting into matrix for matrix operations
  
  A = sigmoid(X%*%t(Theta1))
  
  ma = nrow(A)
  
  A = cbind(rep(1,ma), A)
  
  p <- apply(sigmoid(A%*%t(Theta2)),1,which.max)
  
  p
  
}