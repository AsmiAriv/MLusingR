gaussianKernel <- function(x1, x2, sigma){
  
  #RBFKERNEL returns a radial basis function kernel between x1 and x2
  #   sim = gaussianKernel(x1, x2) returns a gaussian kernel between x1 and x2
  #   and returns the value in sim
  
  # Ensure that x1 and x2 are column vectors
  
  x1 <- as.matrix(x1)
  x2 <- as.matrix(x2)
  
  sim <- 0
  
  sim = exp(-(t(x1-x2)%*%(x1-x2))/(2*sigma^2))
  
  sim
  
}