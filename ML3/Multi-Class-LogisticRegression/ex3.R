#This file uses the following functions

# To run this script please ensure to store these functions in 
# the same folder as this file and set this folder as working directory
 
#     lrCostFunction.R (logistic regression cost function)
#     lrGradFunction.R (logistic regression gradient function)
#     sigmoid.R
#     oneVsAll.R
#     predictOneVsAll.R
#     displayData.R


#Setting the working directory
setwd("C:/R/MachineLearning/ML3/Multi-Class-LogisticRegression")

#Initializing
rm(list=ls()) #Using this code will remove all the objects from R session


#Loading sigmoid function
source('sigmoid.R')

#Loading cost function
source('lrCostFunction.R')

#Loading gradThetaNewton function
source('lrGradFunction.R')

#Loading cost function with regularization
source('oneVsAll.R')

#Loading predictOneVsAll function
source('predictOneVsAll.R')

#Loading displayData function
source('displayData.R')

# Setup the parameters you will use for this part of the exercise
input_layer_size  = 400  # 20x20 Input Images of Digits
num_labels = 10          # 10 labels, from 1 to 10   
# (note that we have mapped "0" to label 10)




library(R.matlab) #Required for reading .mat file

# Load Training Data
data <- readMat('ex3data1.mat') # training data stored in arrays X, y

X <- data[[1]]
y <- data[[2]]

dim(X)
dim(y)

m <- nrow(X)

# Randomly select 100 data points to display
rand_indices <- sample(1:m,100)

sel = X[rand_indices,]

displayData(sel)

#Training One-vs-All Logistic Regression to calculate theta's for each class

lambda = 0.1

all_theta = oneVsAll(X, y, num_labels, lambda)


#Predicting for One-Vs-All

pred = predictOneVsAll(all_theta, X)

cat('\nTraining Set Accuracy: \n', mean((pred == y)) * 100)

