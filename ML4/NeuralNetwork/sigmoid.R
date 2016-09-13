sigmoid <- function(z){
  z <- as.matrix(z)
  g = matrix(rep(0, nrow(z)*ncol(z)),nrow(z))
  
  
  g <- 1/ (1 + exp(-z))

g
  
}