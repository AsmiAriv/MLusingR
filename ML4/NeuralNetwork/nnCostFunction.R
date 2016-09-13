nnCostFunction <- function(nn_params,input_layer_size,hidden_layer_size,
                           num_labels,X, y, lambda){
  
  #NNCOSTFUNCTION Implements the neural network cost function for a two layer
  #neural network which performs classification
  #   NNCOSTFUNCTON(nn_params, hidden_layer_size, num_labels, ...
  #   X, y, lambda) computes the cost and gradient of the neural network. 
  #   The parameters for the neural network are "unrolled" into the vector
  #   nn_params and need to be converted back into the weight matrices. 
   
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
  J <- 0
  
  Theta1_grad = matrix(rep(0,length(Theta1)),nrow(Theta1))
  Theta2_grad = matrix(rep(0,length(Theta2)),nrow(Theta2))
  
  X = as.matrix(X) #Converting into matrix for matrix operations
  X = cbind(rep(1,m),X) #Adding bias
  
  A = sigmoid(X%*%t(Theta1))
  
  ma = nrow(A)
  
  A = cbind(rep(1,ma), A) #Adding bias
  
  for (i in 1:num_labels){
  
  
    
    J1 <- (1/m)*(-t(as.numeric(y==i))%*%log(sigmoid(A%*%(t(Theta2)[,i])))-
                   (t(1-as.numeric(y==i))%*%log(1-sigmoid(A%*%(t(Theta2)[,i]))))) 
    
    J <- J + J1  
  }
  
  temp1 = Theta1
  temp2 = Theta2
  temp1[,1] = 0;
  temp2[,1] = 0;
  J = J+(lambda/(2*m))*(sum(sum(temp1*temp1))+ sum(sum(temp2*temp2)))
  J

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
  
  list(J=J,grad=grad)
  
  }


