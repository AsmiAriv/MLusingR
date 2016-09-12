plotDecisionBoundary <- function(theta, X, y){
  
  plotData(X[,2:3], y) #Plot data
  
  if (dim(X)[2] <= 3) {
  
  #Only need 2 points to define a line, so choose two endpoints
  plot_x = c(min(X[,2])-2,  max(X[,2])+2);
  
  #Calculate the decision boundary line
  plot_y = (-1/theta[3])*(theta[2]*plot_x + theta[1]);
  
  #Plot the decision line
  lines(plot_x,plot_y, col="green")
  
  #Legend, specific for the exercise
  legend("topright",legend=c('Admitted', 'Not admitted', 'Decision Boundary'),pch = c(3, 19, 22), col=c("red","yellow","green"))
  
    }

  else {
    #Here is the grid range
  u = seq(-1, 1.5, length.out=50)
  v = seq(-1, 1.5, length.out=50)
  
  z = matrix(rep(0,length(u)*length(v)),length(u))
  
  #Evaluate z = theta*x over the grid
  
  for (i in 1:length(u)){
    for (j in 1:length(v)){
      z[i,j] = mapFeature(u[i], v[j])%*%theta;
      }
    }
  z = t(z) #important to transpose z before calling contour
  
  #Plot decision boundary using contour
   
  contour(u, v, z,add=T,col="green")
  legend("topright",legend=c('Admitted', 'Not admitted', 'Decision Boundary'),pch = c(3, 19, 22),col=c("red","yellow","green"))
  }
  
}