#Set the working directory

setwd("C:/R/MachineLearning/ML1/SimpleRegression")

#Loading the data 
data <- read.table('ex1data1.txt',sep=",")
X <- data[,1]
y <- data[,2]

#plotting the data
plot(X,y, col="red")

#Adding a column for intercept to the predictors X
X <- cbind(rep(1,dim(data)[1]),data[,1])

#Initializing the fitting parameters
theta <- c(0,0)

#Initializing the gradient descent settings
iterations = 1500
alpha = 0.01

#Loading cost function
source('computeCost.R')

#Loading gradient descent function
source('gradientDescent.R')


#compute and display initial cost
computeCost(X, y, theta)

#run gradient descent

theta = gradientDescent(X, y, theta, alpha, iterations)

#print theta to screen
cat("Theta found by gradient descent:","\n",theta[1],"\n",theta[2])



#Plotting the linear fit
plot(X[,2],y, col="red")
lines(X[,2], X%*%theta, type="l")

# Predict values for population sizes of 35,000 and 70,000
predict1 = c(1, 3.5) %*% theta
predict2 = c(1, 7) %*% theta

#Printing the predicted values
cat("For population = 35,000, we predict a profit of:",predict1*10000)

cat('For population = 70,000, we predict a profit of:',predict2*10000)


#Visualizing J(theta0, theta1)

#Creating grid over which to calculate J

theta0_vals = seq(-10, 10, length.out = 100)
theta1_vals = seq(-1, 4, length.out = 100)

#Initializing J_vals to a matrix of 0's

J_vals = matrix(c(rep(0,length(theta0_vals)*length(theta1_vals))),length(theta0_vals))

#Fill out J-vals
for (i in 1:length(theta0_vals)){
for (j in 1:length(theta1_vals)){
t = c(theta0_vals[i], theta1_vals[j])    
J_vals[i,j] = computeCost(X, y, t)
 }
}

#To create a surface plot 
#We can use the function persp()
persp(theta0_vals, theta1_vals, J_vals,xlab="theta0", ylab="theta1",theta = -50,expand = 0.5, col = "lightblue",
       shade = 0.75, ticktype = "detailed")

#Now we need to create contour plot
#We can use the contour() function 
plot(theta0_vals, theta1_vals,type="n", xlab=expression(theta[0]), ylab=expression(theta[1]))
contour(theta0_vals, theta1_vals, J_vals, method="edge", nlevels = 30,
        col = J_vals[,1], lty = "solid", add = TRUE
)
points(theta[1], theta[2], col="red", pch=3)

























