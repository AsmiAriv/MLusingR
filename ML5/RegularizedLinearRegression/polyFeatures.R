polyFeatures <- function(X, p){
  
  #POLYFEATURES Maps X (1D vector) into the p-th power
  #   [X_poly] = POLYFEATURES(X, p) takes a data matrix X (size m x 1) and
  #   maps each example into its polynomial features where
  #   X_poly[i, ] = [X[i] X[i].^2 X[i].^3 ...  X[i].^p];
  
  X <- as.matrix(X)
  m = nrow(X)
  X_poly = matrix(rep(0,m*p),m)
  
  for (i in 1:p){ 
  X_poly[,i] = X^i
  
  }
 
  X_poly
   
}