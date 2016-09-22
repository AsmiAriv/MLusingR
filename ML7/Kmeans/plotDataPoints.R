plotDataPoints <- function(X, idx, K){
  
  #PLOTDATAPOINTS plots data points in X, coloring them so that those with the same
  #index assignments in idx have the same color
  #   PLOTDATAPOINTS(X, idx, K) plots data points in X, coloring them so that those 
  #   with the same index assignments in idx have the same color
  
  # Create palette
  palette = rainbow(K)
  colors = palette[idx]
  
  #Plot the data
  points(X[,1],X[,2],col=colors, lwd=1, pch=1)
  
  
}