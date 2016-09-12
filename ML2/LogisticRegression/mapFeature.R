mapFeature <- function(X1, X2,degree=6){
  
#MAPFEATURE Feature mapping function to polynomial features
#MAPFEATURE(X1, X2, degree) maps the two input features
#to quadratic features used in the regularization exercise.
#Returns a new feature array with more features, comprising of 
#X1, X2, X1.^2, X2.^2, X1*X2, X1*X2.^2, etc..
#Inputs X1, X2 must be the same size

  X1 <- as.matrix(X1)
  X2 <- as.matrix(X2)
  out = matrix(rep(1,length(X1[,1])))
  
  for (i in 1:degree){
  for (j in 0:i){
  out = cbind(out,(X1^(i-j))*(X2^j))
       }
  
    }
 out 
}