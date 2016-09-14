plotFit <- function(min_x, max_x, mu, sigma, theta, p){
  
  #PLOTFIT Plots a learned polynomial regression fit over an existing figure.
  #Also works with linear regression.
  #   PLOTFIT(min_x, max_x, mu, sigma, theta, p) plots the learned polynomial
  #   fit with power p and feature normalization (mu, sigma).
  
  
  # We plot a range slightly bigger than the min and max values to get
  # an idea of how the fit will vary outside the range of the data points
  
  x = t(t(seq(from=min_x - 15,to=max_x + 25, by=0.05 )))
  
  #Map the X values 
  X_poly = polyFeatures(x, p);
    
for(i in 1:p){
  X_poly[,i] = X_poly[,i]-mu[i]
  X_poly[,i] = X_poly[,i]/sigma[i]
}


  #Add ones
  X_poly = cbind(rep(1,nrow(x)), X_poly)
  
  #Plot
  #plot(x, X_poly%*%theta, col="red", pch=3)
  lines(x, X_poly%*%theta, col="blue",lty="dashed", lwd=2)
  
  
}