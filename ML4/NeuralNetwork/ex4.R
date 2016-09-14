#This file uses the following functions

# To run this script please ensure to store these functions in 
# the same folder as this file and set this folder as working directory

#     checkNNGradients.R (For validating nnCostFunction function)
#     computeNumericalGradient.R (For calculating numerical gradient)
#     sigmoid.R (Activation function)
#     sigmoidGradient.R (gradient of activation function)
#     debugInitializeWeights.R (For debugging weights)
#     nnCostFunction.R (For calculating cost and gradient)
#     randInitializeWeights.R (For random initialization of weights)
#     predict.R (For predicting the output)
#     displayData.R (For displaying the data and weights in a grid)
#     nnCostOptimFunction.R (For using optimizer in R to optimize weights at minimized cost)
#     nnGradOptimFunction.R (For using optimizer in R to optimize weights at minimized cost)


#Setting the working directory
setwd("C:/R/MachineLearning/ML4/NeuralNetwork")

#Initializing
rm(list=ls()) #Using this code will remove all the objects from R session


#Loading sigmoid function
source('sigmoid.R')

#Loading sigmoidGradient function
source('sigmoidGradient.R')

#Loading neural network cost function
source('nnCostFunction.R')

#Loading checkNNGradients function
source('checkNNGradients.R')

#Loading computeNumericalGradient function
source('computeNumericalGradient.R')

#Loading debugInitializeWeights function
source('debugInitializeWeights.R')

#Loading randInitializeWeights function
source('randInitializeWeights.R')

#Loading predict function
source('predict.R')

#Loading data display function
source('displayData.R')

#Loading nnCostOptimFunction function
source('nnCostOptimFunction.R')

#Loading nnGradOptimFunction function
source('nnGradOptimFunction.R')


# Setup the parameters you will use for this part of the exercise
input_layer_size  = 400  # 20x20 Input Images of Digits
hidden_layer_size = 25   # 25 hidden units
num_labels = 10          # 10 labels, from 1 to 10   
# (note that we have mapped "0" to label 10)



library(R.matlab) #Required for reading .mat file

# Load Training Data
data <- readMat('ex4data1.mat') # training data stored in arrays X, y

X <- data[[1]]
y <- data[[2]]

dim(X)
dim(y)

m <- nrow(X)

# Randomly select 100 data points to display
rand_indices <- sample(1:m,100)

sel = X[rand_indices,]

displayData(sel)


#Loading parameters

par <- readMat('ex4weights.mat')

Theta1 <- par[[1]]
Theta2 <- par[[2]]

dim(Theta1)
dim(Theta2)

#Unroll parameters 
nn_params = c(as.vector(Theta1) , as.vector(Theta2))


#Weight regularization parameter (we set this to 0 here).
lambda = 0

cost = nnCostFunction(nn_params, input_layer_size, hidden_layer_size,num_labels, X, y, lambda)

J = cost$J

cat('Cost at parameters (loaded from ex4weights):', 
      '\n(this value should be about 0.287629)\n', J)


#Weight regularization parameter (we set this to 1 here).
lambda = 1

cost = nnCostFunction(nn_params, input_layer_size, hidden_layer_size,num_labels, X, y, lambda)

J = cost$J

cat('Cost at regularized parameters (loaded from ex4weights):', 
    '\n(this value should be about 0.383770)\n', J)

#Evaluating Sigmoid Gradient function
g = sigmoidGradient(c(1, -0.5, 0, 0.5, 1))
cat('Sigmoid gradient evaluated at [1 -0.5 0 0.5 1]:\n')
cat(g)
cat('\n\n')

#Initializing  Neural Network Pameters 

initial_Theta1 = randInitializeWeights(input_layer_size, hidden_layer_size)
initial_Theta2 = randInitializeWeights(hidden_layer_size, num_labels)

#Unroll parameters
initial_nn_params = c(as.vector(initial_Theta1) , as.vector(initial_Theta2))

#Implement Backpropagation

checkNNGradients()
                      

#Check gradients by running checkNNGradients
lambda = 3
checkNNGradients(lambda)

# Also output the costFunction debugging values
debug_J  = nnCostFunction(nn_params, input_layer_size,hidden_layer_size, num_labels, X, y, lambda);

cat('\n\nCost at (fixed) debugging parameters (w/ lambda = 10):',
    '\n(this value should be about 0.576051)\n\n', debug_J$J)


#Training Neural Network
# Set regularization parameter lambda to 1 (you should vary this)
lambda = 1

#Compute and display Weights and cost using optim() as optimizer
#num_iter = 100

costh <- optim(par=initial_nn_params, fn=nnCostOptimFunction, gr=nnGradOptimFunction, 
               method="BFGS", input_layer_size=input_layer_size,
               hidden_layer_size=hidden_layer_size, num_labels=num_labels,
               X=X,y=y,lambda=lambda, control = list(maxit=100))

nn_params <- costh$par
#grad <- costh$gradient
cost <- costh$value


cat('Cost at Weights found after optimization:', '\n', cost)

# Reshape nn_params back into the parameters Theta1 and Theta2, the weight matrices
# for our 2 layer neural network

Theta1 <- nn_params[1:(hidden_layer_size*(input_layer_size + 1))]
dim(Theta1) <- c(hidden_layer_size, (input_layer_size + 1))

Theta2 <- nn_params[(1 + (hidden_layer_size * (input_layer_size + 1))):length(nn_params)]
dim(Theta2) <- c(num_labels, (hidden_layer_size + 1))

#Vizualising hidden layer's Weights

displayData(Theta1[, 2:ncol(Theta1)])


#Prediction
pred = predict(Theta1, Theta2, X)

cat('The train accuracy should be about 95.3%','\n',
    '(this may vary by about 1% due to the random initialization)')
cat('Training Set Accuracy:\n',mean((pred == y))*100)


#Compute and display Weights and cost using nlminb() as optimizer
#num_iter = 100

costh <- nlminb(start=initial_nn_params, objective=nnCostOptimFunction, 
                gradient=nnGradOptimFunction, input_layer_size=input_layer_size,
                hidden_layer_size=hidden_layer_size, num_labels=num_labels,
                X=X,y=y,lambda=lambda, control = list(iter.max=100))

nn_params <- costh$par
#grad <- costh$gradient
cost <- costh$objective


cat('Cost at Weights found after optimization:', '\n', cost)

# Reshape nn_params back into the parameters Theta1 and Theta2, the weight matrices
# for our 2 layer neural network

Theta1 <- nn_params[1:(hidden_layer_size*(input_layer_size + 1))]
dim(Theta1) <- c(hidden_layer_size, (input_layer_size + 1))

Theta2 <- nn_params[(1 + (hidden_layer_size * (input_layer_size + 1))):length(nn_params)]
dim(Theta2) <- c(num_labels, (hidden_layer_size + 1))

#Vizualising hidden layer's Weights

displayData(Theta1[, 2:ncol(Theta1)])


#Prediction
pred = predict(Theta1, Theta2, X)

cat('The train accuracy should be about 95.3%','\n',
    '(this may vary by about 1% due to the random initialization)')
cat('Training Set Accuracy:\n',mean((pred == y))*100)

#Both optim() and nlminb() take the same amount of time
#Their accuracies are similar as well

