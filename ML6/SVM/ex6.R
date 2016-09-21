#This file uses the following functions

# To run this script please ensure to store these functions in 
# the same folder as this file and set this folder as working directory

#     gaussianKernel.R (For using gaussian kernel)
#     linearKernel.R (For using linear kernel)
#     dataset3Params.R (For creating polynomial features)
#     processEmail.R (For pre-processing an e-mail for analysis)
#     emailFeatures.R (For extracting e-mail features)
#     svmTrain.R (for training the model)
#     plotData.R (For plotting the data)
#     visualizeBoundaryLinear.R (For visualizing linear decision boundary)
#     visualizeBoundary.R (For visualizing non-linear decision boundary)
#     svmPredict.R (For prediction)


#Setting the working directory
setwd("C:/R/MachineLearning/ML6/SVM")

#Initializing
rm(list=ls()) #Using this code will remove all the objects from R session

#loading gaussianKernel.R
source('gaussianKernel.R')

#loading linearKernel.R
source('linearKernel.R')

#loading dataset3Params.R
source('dataset3Params.R')

#loading processEmail.R
source('processEmail.R')

#loading emailFeatures.R
source('emailFeatures.R')

#loading svmTrain.R
source('svmTrain.R')

#loading plotData.R
source('plotData.R')

#loading visualizeBoundaryLinear.R
source('visualizeBoundaryLinear.R')

#loading visualizeBoundary.R
source('visualizeBoundary.R')

#loading svmPredict.R
source('svmPredict.R')

library(R.matlab) #Required for reading .mat file

# Load Training Data
data <- readMat('ex6data1.mat') # training data stored in arrays X, y

X <- data[[1]]
y <- data[[2]]

# Plot training data
plotData(X, y)

#Training Linear SVM
C = 1
model = svmTrain(X, y, C, 0,linearKernel, 1e-3, 20)
visualizeBoundaryLinear(X, y, model)

predictions <- svmPredict(model,X)

error <- mean(predictions!=y)
cat('error should be around 0.019608 \n',error)


#Implementing Gaussian Kernel
x1 = c(1, 2, 1); x2 = c(0, 4, -1); sigma = 2;
sim = gaussianKernel(x1, x2, sigma)

cat('Gaussian Kernel between x1 = [1; 2; 1], x2 = [0; 4; -1], sigma = 2 :',
      '\n','(this value should be about 0.324652)\n', sim)



#Visualizing Dataset 2
data <- readMat('ex6data2.mat') # training data stored in arrays X, y

X <- data[[1]]
y <- data[[2]]

plotData(X,y)

#Training SVM with RBF Kernel (Dataset 2)

#Setting Parameters
C = 1; sigma = 0.1

#We set the tolerance and max_passes lower here so that the code will run
# faster. However, in practice, you will want to run the training to
# convergence

model= svmTrain(X, y, C, sigma,gaussianKernel) 
visualizeBoundary(X, y, model)

#Visualizing Dataset 3
data <- readMat('ex6data3.mat')

X <- data[[1]]
y <- data[[2]]

Xval <- data[[4]]
yval <- data[[3]]

#Plot training data
plotData(X, y)

#Plot validation data
plotData(Xval, yval)

#Training SVM with RBF Kernel (Dataset 3)


#Try different SVM Parameters here
Csigma = dataset3Params(X, y, Xval, yval) #This may take a while
C = Csigma$C
sigma = Csigma$sigma

#Train the SVM
model= svmTrain(X, y, C, sigma,gaussianKernel)
visualizeBoundary(X, y, model)









