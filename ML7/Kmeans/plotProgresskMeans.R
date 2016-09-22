plotProgresskMeans <- function(X, centroids, previous, idx, K, i){
  
  #PLOTPROGRESSKMEANS is a helper function that displays the progress of 
  #k-Means as it is running. It is intended for use only with 2D data.
  #   PLOTPROGRESSKMEANS(X, centroids, previous, idx, K, i) plots the data
  #   points with colors assigned to each centroid. With the previous
  #   centroids, it also plots a line between the previous locations and
  #   current locations of the centroids.
  
  #Plot the examples
  plotDataPoints(X, idx, K)
  
  #Plot the centroids as black x's
  points(centroids[,1], centroids[,2],col="black",pch=4,lty=1,lwd=6)
  
  #Plot the history of the centroids with lines
  for(j in 1:nrow(centroids)){
    drawLine(centroids[j,], previous[j,])
  }
  
  
  
}