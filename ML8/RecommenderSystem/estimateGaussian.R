estimateGaussian <- function(X){
  #ESTIMATEGAUSSIAN This function estimates the parameters of a 
  #Gaussian distribution using the data in X
  #   [mu sigma2] = estimateGaussian(X), 
  #   The input X is the dataset with each n-dimensional data point in one row
  #   The output is an n-dimensional vector mu, the mean of the data set
  #   and the variances sigma^2, an n x 1 vector
  
  m <- nrow(X)
  n <- ncol(X)
  
  mu <- rep(0,n)
  sigma2 <- rep(0,n)
  
  mu <- colMeans(X)
  sigma2 <- (m-1)*(apply(X,2,var))/m
  
  list(mu=mu,sigma2=sigma2)
  
  
}