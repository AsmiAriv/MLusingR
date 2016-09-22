featureNormalize <- function(X){
  
#FEATURENORMALIZE Normalizes the features in X 
#FEATURENORMALIZE(X) returns a normalized version of X where
#the mean value of each feature is 0 and the standard deviation
#is 1. This is often a good preprocessing step to do when
#working with learning algorithms.

  
  X_norm = as.matrix(X)
  mu = rep(0, dim(X)[2])
  sigma = rep(0, dim(X)[2])
  
  for (i in 1:dim(X)[2]){
  mu[i] = mean(X[,i])
  sigma[i] = sd(X[,i])
  X_norm[,i] = (X_norm[,i]-mu[i])/sigma[i]
  }
  
 list(X=X_norm, mu=mu, sigma=sigma) 
}