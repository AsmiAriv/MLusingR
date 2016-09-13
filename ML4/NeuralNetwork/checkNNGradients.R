checkNNGradients <- function(lambda){
  
  #CHECKNNGRADIENTS Creates a small neural network to check the
  #backpropagation gradients
  #   CHECKNNGRADIENTS(lambda) Creates a small neural network to check the
  #   backpropagation gradients, it will output the analytical gradients
  #   produced by your backprop code and the numerical gradients (computed
  #  using computeNumericalGradient). These two gradient computations should
  #   result in very similar values.
  
  input_layer_size = 3
  hidden_layer_size = 5
  num_labels = 3
  m = 5
  lambda = lambda
  # We generate some 'random' test data
  Theta1 = debugInitializeWeights(hidden_layer_size, input_layer_size)
  Theta2 = debugInitializeWeights(num_labels, hidden_layer_size)
  # Reusing debugInitializeWeights to generate X
  X  = debugInitializeWeights(m, input_layer_size - 1)
  y  = 1 + t(1:m%%num_labels)

# Unroll parameters
nn_params = c(as.vector(Theta1), as.vector(Theta2))

# Short hand for cost function
costFunc = function(p){
  
  cost = nnCostFunction(p, input_layer_size, 
                          hidden_layer_size, num_labels, 
                          X, y, lambda)

  J = cost$J
  grad = cost$grad
  list(J=J, grad=grad)
  }
  
  res1 <- costFunc(nn_params)
  
  grad <- res1$grad
  J <- res1$J
  
  numgrad = computeNumericalGradient(costFunc, nn_params)
  
  df <- data.frame(numgrad=numgrad,grad=grad)
  print(df)
  
  cat('The above two columns you get should be very similar.\n',
      '(Left-Your Numerical Gradient, Right-Analytical Gradient)\n\n')
  
  #A <- as.matrix((df[,2]-df[,1]))
  #B <- as.matrix((df[,2]+df[,1]))
  
  diff = norm(as.matrix(svd(numgrad-grad)$d))/norm(as.matrix(svd(numgrad+grad)$d))
  cat('If your backpropagation implementation is correct, then \n',
      'the relative difference will be small (less than 1e-9). \n',
      '\nRelative Difference:',diff)
  
  #list(df=df, diff=diff)
  #df
  
}