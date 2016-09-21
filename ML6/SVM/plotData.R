plotData <- function(X, y){
  
#PLOTDATA Plots the data points X and y into a new figure 
#PLOTDATA(x,y) plots the data points with + for the positive examples
#and o for the negative examples. X is assumed to be a Mx2 matrix.
#Create New Figure
  
  pos <- which(y==1)
  neg <- which(y==0)
  
  plot(X[pos,1],X[pos,2], col="red",pch=3)
  points(X[neg,1],X[neg,2], col="yellow", pch=19)
  
  
  
}