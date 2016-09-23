#This file uses the following functions

# To run this script please ensure to store these functions in 
# the same folder as this file and set this folder as working directory

#     estimateGaussian.R (For estimating mu and variance)
#     selectThreshold.R (For selecting the threshold value of anomaly probability)
#     multivariateGaussian.R (For calculating probability density function)
#     visualizeFit.R (For visualizing the data)


#Setting the working directory
setwd("C:/R/MachineLearning/ML8/RecommenderSystem")

#Initializing
rm(list=ls()) #Using this code will remove all the objects from R session

#loading estimateGaussian.R
source('estimateGaussian.R')

#loading selectThreshold.R
source('selectThreshold.R')

#loading multivariateGaussian.R
source('multivariateGaussian.R')

#loading visualizeFit.R
source('visualizeFit.R')


library(R.matlab) #Required for reading .mat file

# Load an example dataset that we will be using
data <- readMat('ex8data1.mat') 
X <- data[[1]]

Xval <- data[[2]]
yval <- data[[3]]

#Visualize the example dataset
plot(X[,1],X[,2], col="blue", pch=4, xlim = c(0,30), ylim = c(0,30),
     xlab='Latency (ms)',ylab = 'Throughput (mb/s)')

#Estimate the dataset statistics
#Estimate mu and sigma2
mvar <- estimateGaussian(X)

mu <- mvar$mu
sigma2 <- mvar$sigma2

#  Returns the density of the multivariate normal at each data point (row) 
#  of X

p = multivariateGaussian(X, mu, sigma2)


#Visualize the fit
visualizeFit(X,  mu, sigma2) #Need to work on this part, contour in R for matrices

#Find Outliers
#  Now we will find a good epsilon threshold using a cross-validation set
#  probabilities given the estimated Gaussian distribution

pval = multivariateGaussian(Xval, mu, sigma2)

epF1 = selectThreshold(yval, pval)

epsilon <- epF1$bestEpsilon
F1 <- epF1$bestF1

cat('Best epsilon found using cross-validation: \n', epsilon)
cat('Best F1 on Cross Validation Set:  \n', F1)
cat('(you should see a value epsilon of about 8.99e-05)\n\n')


#Find the outliers in the training set and plot the
outliers = which(p < epsilon)

#Draw a red circle around those outliers

points(X[outliers, 1], X[outliers, 2],col="red", lwd=2, pch=1)

#Multidimensional Outliers
#We will now use the code from the previous part and apply it to a 
#  harder problem in which more features describe each datapoint and only 
#  some features indicate whether a point is an outlier.

#Loading the dataset
data <- readMat('ex8data2.mat') 
X <- data[[1]]

Xval <- data[[2]]
yval <- data[[3]]

#Apply the same steps to the larger dataset
mvar <- estimateGaussian(X)

mu <- mvar$mu
sigma2 <- mvar$sigma2

#Training set 
p = multivariateGaussian(X, mu, sigma2)

#Cross-validation set
pval = multivariateGaussian(Xval, mu, sigma2)

#Find the best threshold
epF1 = selectThreshold(yval, pval);

epsilon <- epF1$bestEpsilon
F1 <- epF1$bestF1

cat('Best epsilon found using cross-validation: \n', epsilon)
cat('Best F1 on Cross Validation Set:  \n', F1)
cat('# Outliers found:\n', sum(p < epsilon))
cat('(you should see a value epsilon of about 1.38e-18)\n\n')



