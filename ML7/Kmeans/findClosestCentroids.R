findClosestCentroids <- function(X, centroids){
  
  #FINDCLOSESTCENTROIDS computes the centroid memberships for every example
  #   idx = FINDCLOSESTCENTROIDS (X, centroids) returns the closest centroids
  #   in idx for a dataset X where each row is a single example. idx = m x 1 
  #   vector of centroid assignments (i.e. each entry in range [1..K])
  
  #Converting for matrix operations
  X <- as.matrix(X)
  centroids <- as.matrix(centroids)
  
  # Set K
  K = nrow(centroids)
  
  #Need to return the following vector of index of minimum distance
  idx = rep(0,nrow(X))
  
  for(i in 1:nrow(X)){
    sim = rep(0,K)
    
    for(j in 1:K){
      sim[j] = t(X[i,]-centroids[j,])%*%(X[i,]-centroids[j,])  
    }
    
    idx[i] <- which.min(sim)
  }
 idx 
}