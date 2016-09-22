pca <- function(X){
  
  #PCA Run principal component analysis on the dataset X
  #   [U, S, X] = pca(X) computes eigenvectors of the covariance matrix of X
  #   Returns the eigenvectors U, the eigenvalues (on diagonal) in S
  
  
  # Useful values
  m = nrow(X)
  n = ncol(X)
  
  U = matrix(rep(0,n*n),n)
  S = matrix(rep(0,n*n),n)
  
  
  sigma = (1/m)*(t(X)%*%X)
  
  US = svd(sigma)
  
  diag(S) = US$d
  U = US$u
  
  list(U = U, S = S)
  
}