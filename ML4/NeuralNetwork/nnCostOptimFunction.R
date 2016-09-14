nnCostOptimFunction <- function(nn_params,input_layer_size,hidden_layer_size,
                           num_labels,X, y, lambda){
  
  #NNCOSTOPTIMFUNCTION Implements the neural network cost function for a two layer
  #This Function has been written to optimize weight parameters using optimizer
  #neural network which performs classification
  #   NNCOSTOPTIMFUNCTON(nn_params, hidden_layer_size, num_labels, ...
  #   X, y, lambda) computes the cost of the neural network. 
  #   The parameters for the neural network are "unrolled" into the vector
  #   nn_params and need to be converted back into the weight matrices. 
 


  # Reshape nn_params back into the parameters Theta1 and Theta2, the weight matrices
  # for our 2 layer neural network
  
  Theta1 <- nn_params[1:(hidden_layer_size*(input_layer_size + 1))]
  dim(Theta1) <- c(hidden_layer_size, (input_layer_size + 1))
  
  Theta2 <- nn_params[(1 + (hidden_layer_size * (input_layer_size + 1))):length(nn_params)]
  dim(Theta2) <- c(num_labels, (hidden_layer_size + 1))
  
  
  #Setup some useful variables
  m <- nrow(X)
  J <- 0
  
   
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

  
  }


