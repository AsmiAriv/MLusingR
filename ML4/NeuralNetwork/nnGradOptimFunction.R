nnGradOptimFunction <- function(nn_params,input_layer_size,hidden_layer_size,
                           num_labels,X, y, lambda){
  
  #NNGRADOPTIMFUNCTION Implements the neural network grad function for a two layer
  #neural network which performs classification
  #This function has been written to optimize weights using optimizer
  #   NNGRADOPTIMTFUNCTON(nn_params, hidden_layer_size, num_labels, ...
  #   X, y, lambda) computes the gradient of the neural network. 
  #   The returned parameter grad should be a "unrolled" vector of the
  #   partial derivatives of the neural network.
  #
  
  # Reshape nn_params back into the parameters Theta1 and Theta2, the weight matrices
  # for our 2 layer neural network
  
  Theta1 <- nn_params[1:(hidden_layer_size*(input_layer_size + 1))]
  dim(Theta1) <- c(hidden_layer_size, (input_layer_size + 1))
  
  Theta2 <- nn_params[(1 + (hidden_layer_size * (input_layer_size + 1))):length(nn_params)]
  dim(Theta2) <- c(num_labels, (hidden_layer_size + 1))
  
  
  #Setup some useful variables
  m <- nrow(X)
    
  Theta1_grad = matrix(rep(0,length(Theta1)),nrow(Theta1))
  Theta2_grad = matrix(rep(0,length(Theta2)),nrow(Theta2))
  
  X = as.matrix(X) #Converting into matrix for matrix operations
  X = cbind(rep(1,m),X) #Adding bias
  
   
  temp1 = Theta1
  temp2 = Theta2
  temp1[,1] = 0;
  temp2[,1] = 0;
  
  tp <- matrix(1:num_labels,1)
  yt <- matrix(rep(tp,m),m,byrow=T)
  yt = 1*(yt == as.vector(y))
  
  A1 = X
  Z2 = A1%*%t(Theta1)
  A2 = sigmoid(Z2)
  ma = nrow(A2)
  A2 = cbind(rep(1,ma), A2)
  Z3 = A2%*%t(Theta2)
  A3 = sigmoid(Z3)
  d3 = A3-yt
  d2 = (d3%*%Theta2)*(A2*(1-A2))
  end = nrow(t(d2))
  
  Theta1_grad = Theta1_grad + (t(d2)%*%A1)[2:end,];
  Theta2_grad = Theta2_grad + t(d3)%*%(A2);
                               
  Theta1_grad = (1/m)*(Theta1_grad + lambda*temp1);
  Theta2_grad = (1/m)*(Theta2_grad + lambda*temp2);
  
  
  grad <- c(as.vector(Theta1_grad),as.vector(Theta2_grad)) #Unrolling gradients
  
 grad
  
  }


