#Set the working directory
setwd("C:/R/MachineLearning/ML1/MultiRegression")

#Loading the data 
data <- read.table('ex1data2.txt',sep=",")
X <- data[,1:2]
y <- data[,3]
m = length(y)

#Loading cost function
source('computeCostMulti.R')

#Loading gradient descent function
source('gradientDescentMulti.R')

#Loading normal equation function
source('normalEqn.R')

#Loading feature normalizing function
source('featureNormalize.R')

#Scale features and set them to zero mean
fn <- featureNormalize(X)
X <- fn$X
mu <- fn$mu
sigma <- fn$sigma

#Adding a column for intercept to the predictors X
X <- cbind(rep(1,m),X)
X <- as.matrix(X)

#Choose some alpha value
alpha = 0.01
num_iters = 3000

#Initializing the fitting parameters
theta <- rep(0,dim(X)[2])

gd <- gradientDescentMulti(X, y, theta, alpha, num_iters)

theta <- gd$theta
#The final value should be approx (340413,110631,-6650)

J_history <- gd$J_history



#Plot the convergence graph
plot(1:length(J_history), J_history)

#Display gradient descent's result
cat('Theta computed from gradient descent:',theta)

#Predicting the price for a 3br flat with size 1650 sq-ft with gradient descent
x1 <- (1650-mu[1])/sigma[1] #Normalising x1
x2 <- (3-mu[2])/sigma[2]    #Normalising x2

price <- c(1, x1, x2)%*%theta

cat('Predicted price of a 1650 sq-ft, 3 br house using gd is:',price)

#Now calculate theta using normal equation with original data
#Loading original data and adding intercept term to X
data <- read.table('ex1data2.txt',sep=",")
X <- data[,1:2]
y <- data[,3]
X <- cbind(rep(1,m),X)
X <- as.matrix(X) #Converting X into a matrix

#Calculating theta
theta <- normalEqn(X,y)

#Predicting the price for a 3br flat with size 1650 sq-ft using normal equation
price <- c(1, 1650, 3)%*%theta

cat('Predicted price of a 1650 sq-ft, 3 br house using normal equation is:',price)