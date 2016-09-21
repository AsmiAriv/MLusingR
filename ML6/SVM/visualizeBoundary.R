visualizeBoundary <- function(X, y, model, ...){
  #VISUALIZEBOUNDARY plots a non-linear decision boundary learned by the SVM
  #   VISUALIZEBOUNDARYLINEAR(X, y, model) plots a non-linear decision 
  #   boundary learned by the SVM and overlays the data on it
  
  # Plot the training data on top of the boundary
  plotData(X, y)
  
  # Make classification predictions over a grid of values
  x1plot <- seq(min(X[,1]), max(X[,1]), length.out=100)
  x2plot <- seq(min(X[,2]), max(X[,2]), length.out=100)
  m <- length(x1plot); n=length(x2plot)
  X1 <- matrix(rep(x1plot,each=n),nrow=n)
  X2 <- matrix(rep(x2plot,m),nrow=n)
  vals <- matrix(rep(0,length(X1)),nrow(X1))
  
  for(i in 1:ncol(X1)){
    this_X = cbind(X1[, i], X2[, i])
    vals[, i] = svmPredict(model, this_X)
  }
  
  #Plot the SVM boundary
  contour(X1, X2, vals,add=T, col="blue")
  #persp(as.vector(X1), as.vector(t(X2)), vals,add=T, col="blue")
  }