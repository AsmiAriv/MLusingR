cofiCostFunc <- function(params, Y, R, num_users, num_movies,num_features, lambda){
  
  #COFICOSTFUNC Collaborative filtering cost function
  #   [J, grad] = COFICOSTFUNC(params, Y, R, num_users, num_movies, ...
  #   num_features, lambda) returns the cost for the
  #   collaborative filtering problem.
  
  
  # Unfold the U and W matrices from params
  X <- params[1:(num_movies*num_features)]
  dim(X) <- c(num_movies, num_features)
  
  Theta <- params[(num_movies*num_features+1):length(params)]
  dim(Theta) <- c(num_users, num_features)
  
  J = 0
  
  J = (1/(2))*((X%*%t(Theta) - Y))*(X%*%t(Theta) - Y)
               
               
  J = sum(sum(J*R))
               
  J = J + (lambda/2)*sum(sum((Theta)*(Theta))) + (lambda/2)*sum(sum((X)*(X)))
  
  J
}