cofiGradFunc <- function(params, Y, R, num_users, num_movies,num_features, lambda){
  
  #COFIGRADFUNC Collaborative filtering grad function
  #   grad = COFIGRADFUNC(params, Y, R, num_users, num_movies, ...
  #   num_features, lambda) returns the gradient for the
  #   collaborative filtering problem.
  
  
  # Unfold the U and W matrices from params
  X <- params[1:(num_movies*num_features)]
  dim(X) <- c(num_movies, num_features)
  
  Theta <- params[(num_movies*num_features+1):length(params)]
  dim(Theta) <- c(num_users, num_features)
  
  
  X_grad = matrix(rep(0,length(X)),dim(X))
  Theta_grad = matrix(rep(0,length(Theta)),dim(Theta))
  
  Theta_grad = t(R*((X%*%t(Theta))-Y))%*%X
  X_grad = (R*((X%*%t(Theta))-Y))%*%Theta
                                    
  X_grad =  X_grad + lambda*X
  Theta_grad = Theta_grad + lambda*Theta
  
  grad <- c(as.vector(X_grad),as.vector(Theta_grad))
  
  grad
  
  
}