multivariateGaussian <- function(X, mu, Sigma2){
  
  #MULTIVARIATEGAUSSIAN Computes the probability density function of the
  #multivariate gaussian distribution.
  #    p = MULTIVARIATEGAUSSIAN(X, mu, Sigma2) Computes the probability 
  #    density function of the examples X under the multivariate gaussian 
  #    distribution with parameters mu and Sigma2. If Sigma2 is a matrix, it is
  #    treated as the covariance matrix. If Sigma2 is a vector, it is treated
  #    as the \sigma^2 values of the variances in each dimension (a diagonal
  #    covariance matrix)
  
  #Require MASS package for pseudo inverse function ginv
  library(MASS)
  
  k = length(mu)
  
  #Converting to matrix for matrix operations
  mu <- as.matrix(mu)
  
  #Sigma2 <- diag(Sigma2)
  
  if (is.null(dim(Sigma2))) Sigma2 = diag(Sigma2)
  
  X <- sweep(X,2,t(mu),FUN="-")
  
  p = ((2*pi)^(- k / 2))*(det(Sigma2)^(-0.5))*exp(-0.5*rowSums((X%*%ginv(Sigma2))*X))
  
  p
  
}