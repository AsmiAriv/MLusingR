checkCostFunction <- function(lambda=0){
  
  #CHECKCOSTFUNCTION Creates a collaborative filering problem 
  #to check your cost function and gradients
  #   CHECKCOSTFUNCTION(lambda) Creates a collaborative filering problem 
  #   to check your cost function and gradients, it will output the 
  #   analytical gradients produced by your code and the numerical gradients 
  #   (computed using computeNumericalGradient). These two gradient 
  #   computations should result in very similar values.
  
  # Create small problem
  X_t = matrix(runif(12),4)
  Theta_t = matrix(runif(15),5)
  
  # Zap out most entries
  Y = X_t%*%t(Theta_t)
  yt <- matrix(runif(length(Y)),dim(Y))
  Y[yt > 0.5] = 0
  
  R = matrix(rep(0,length(Y)),dim(Y))
  R[Y != 0] = 1

  # Run Gradient Checking
  X = matrix(rnorm(length(X_t)),dim(X_t))
  Theta = matrix(rnorm(length(Theta_t)),dim(Theta_t))
  num_users = ncol(Y)
  num_movies = nrow(Y)
  num_features = ncol(Theta_t)

# Unroll parameters
params = c(as.vector(X), as.vector(Theta))

# Short hand for cost function
costFunc = function(p){
  
  cost = cofiCostFunc(p, Y, R, num_users, num_movies,num_features, lambda)

  cost
  }
  
  J <- costFunc(params)
  grad <- cofiGradFunc(params,Y, R, num_users, num_movies,num_features, lambda)
  
  numgrad = computeNumericalGradient(costFunc, params)
  
  df <- data.frame(numgrad=numgrad,grad=grad)
  print(df)
  
  cat('The above two columns you get should be very similar.\n',
      '(Your Left-Numerical Gradient, Your Right-Analytical Gradient)\n\n')
  
  diff = norm(as.matrix(svd(numgrad-grad)$d))/norm(as.matrix(svd(numgrad+grad)$d))
  cat('If your implementation is correct, then \n',
      'the relative difference will be small (less than 1e-9). \n',
      '\nRelative Difference:',diff)
  
  
  
}