visualizeFit <- function(X, mu, sigma2){
  
  #VISUALIZEFIT Visualize the dataset and its estimated distribution.
  #   VISUALIZEFIT(X, p, mu, sigma2) This visualization shows you the 
  #   probability density function of the Gaussian distribution. Each example
  #   has a location (x1, x2) that depends on its feature values.
  
  #Creating vectors for meshgrid
  x <- seq(0,35,by=0.5)
  m <- length(x)
  
  X1 <- matrix(rep(x,m),m,byrow=T)
  X2 <- matrix(rep(x,m),m,byrow=F)
  
  Z = multivariateGaussian(cbind(as.vector(X1), as.vector(X2)),mu,sigma2)
  
  dim(Z) <- c(nrow(X1),ncol(X1))
  
  plot(X[, 1], X[, 2],col='blue',pch=4)
  
  #Do not plot if there are infinities
  if (sum(is.infinite(Z)) == 0){
    
    contour(X1,t(X2)
            ,Z, nlevels = 2,add=T, col="blue")
    
  }
  
}