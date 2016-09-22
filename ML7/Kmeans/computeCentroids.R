computeCentroids <- function(X, idx, K){
  
  #COMPUTECENTROIDS returs the new centroids by computing the means of the 
  #data points assigned to each centroid.
  #   centroids = COMPUTECENTROIDS(X, idx, K) returns the new centroids by 
  #   computing the means of the data points assigned to each centroid. It is
  #   given a dataset X where each row is a single data point, a vector
  #   idx of centroid assignments (i.e. each entry in range [1..K]) for each
  #   example, and K, the number of centroids. You should return a matrix
  #   centroids, where each row of centroids is the mean of the data points
  #   assigned to it.
  
  
  # Useful variables
  m = nrow(X)
  n = ncol(X)
  
  #Need to return the following correctly
  centroids = matrix(rep(0, K*n),K)
  
  for(i in 1:K){
    ind = which(idx==i)
    centroids[i,] = colMeans(X[(ind),])
    
  }
  
  centroids
 
}