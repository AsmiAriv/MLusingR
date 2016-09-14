trainLinearReg <- function(X, y, lambda){
  
  #TRAINLINEARREG Trains linear regression given a dataset (X, y) and a
  #regularization parameter lambda
  #Returns the trained parameters theta.
  #It uses optim() from R and 
  #our cost and grad functions: linearRegCostFunction and linearReggradFunction
  
  X <- as.matrix(X)
  # Initialize Theta
  initial_theta = rep(0,ncol(X)) 

  
  #Calculate theta using optim() as optimizer
  #itr_num = 200
  
  costh <- optim(par=initial_theta, fn=linearRegCostFunction, 
                 gr=linearReggradFunction, method="BFGS", X=X,y=y,lambda=lambda, 
                 control = list(maxit=200))
  
  cost <- costh$value
  theta <- costh$par
  
  list(cost=cost, theta=theta)
 
}