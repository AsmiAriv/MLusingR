predictOneVsAll <- function(all_theta, X){
  
  #PREDICT Predict the label for a trained one-vs-all classifier. The labels 
  #are in the range 1..K, where K = nrow(all_theta). 
  #  p = PREDICTONEVSALL(all_theta, X) will return a vector of predictions
  #  for each example in the matrix X. Note that X contains the examples in
  #  rows. all_theta is a matrix where the i-th row is a trained logistic
  #  regression theta vector for the i-th class. You should set p to a vector
  #  of values from 1..K (e.g., p = c(1, 3, 1, 2) predicts classes 1, 3, 1, 2
  #                        %  for 4 examples) 
  
  m <- nrow(X)
  num_labels = nrow(all_theta)
  
  p = c(rep(0,nrow(X)))
  
  X <- cbind(rep(1,m),X)
  
  x <- as.matrix(X) #Converting X into matrix for matrix operation
  
  p <- apply(sigmoid(X%*%t(all_theta)),1,which.max)
  
  p
  
  
  
}