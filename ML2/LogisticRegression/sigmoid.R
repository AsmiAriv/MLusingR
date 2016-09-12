sigmoid <- function(z){
  z <- as.matrix(z)
  g = matrix(rep(0, nrow(z)*ncol(z)),nrow(z))
  
  
  for (i in 1:nrow(z)){
  for (j in 1:ncol(z)){
  g[i,j] = 1/(1+exp(-z[i,j]))
      }
  }
  g
  
}