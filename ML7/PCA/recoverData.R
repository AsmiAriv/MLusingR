recoverData <- function(Z, U, K){
  
  #RECOVERDATA Recovers an approximation of the original data when using the 
  #projected data
  #   X_rec = RECOVERDATA(Z, U, K) recovers an approximation the 
  #   original data that has been reduced to K dimensions. It returns the
  #   approximate reconstruction in X_rec.
  
  X_rec = matrix(rep(0,nrow(Z)*nrow(U)),nrow(Z))
  
  X_rec = Z%*%t(U[,1:K])
  
  X_rec
}