normalEqn <- function(X, y){
  
#NORMALEQN Computes the closed-form solution to linear regression 
#NORMALEQN(X,y) computes the closed-form solution to linear 

X <- as.matrix(X) #Converting into a matrix

#regression using the normal equations.
  
  theta = rep(0, dim(X)[2])
  
  theta = (solve(t(X)%*%X))%*%(t(X))%*%y
  
  
}