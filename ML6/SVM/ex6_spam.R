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

#Email Preprocessing
#Extract Features
con <- file("emailSample1.txt")
file_contents <- readLines(con, encoding="UTF-8")
close(con)
length(file_contents) #Number of sentences

word_indices = processEmail(file_contents)

#Print Stats
cat('Word Indices: \n')
word_indices

#Feature Extraction
features = emailFeatures(word_indices)

#Print Stats
cat('Length of feature vector: \n',length(features))
cat('Number of non-zero entries: \n',sum(features))

#Train Linear SVM for Spam Classification
#load data
data <- readMat('spamTrain.mat') # training data stored in arrays X, y

X <- data[[1]]
y <- data[[2]]

# Plot training data
plotData(X, y)

#Training Linear SVM
C = 0.1
model = svmTrain(X, y, C, 0,linearKernel)

#Prediction
p = svmPredict(model, X)
cat('Training Accuracy: \n', mean((p == y)) * 100)

#Test Spam Classification
data <- readMat('spamTest.mat') # training data stored in arrays X, y

Xtest <- data[[1]]
ytest <- data[[2]]

p = svmPredict(model, Xtest)
cat('Test Accuracy: \n', mean((p == ytest)) * 100)

#Top Predictors of Spam
# Sort the weights and obtain the vocabulary list
model.w <- model$model.w
indx <- order(model.w, decreasing = TRUE)
weight <- sort(model.w,decreasing = TRUE)

vocabList = read.table('vocab.txt',stringsAsFactors=FALSE)

cat('Top predictions of spam \n')

for(i in 1:15){
  
  cat('\n',vocabList[indx[i],2],weight[i])
  
}

#Try Your Own Emails

#Now that you've trained the spam classifier, you can use it on your own
#  emails! In the starter code, we have included spamSample1.txt,
#  spamSample2.txt, emailSample1.txt and emailSample2.txt as examples. 
#  The following code reads in one of these emails and then uses your 
#  learned SVM classifier to determine whether the email is Spam or 
#  Not Spam

# Set the file to be read in (change this to spamSample2.txt,
# emailSample1.txt or emailSample2.txt to see different predictions on
# different emails types). Try your own emails as well!

filename = 'spamSample1.txt'

con <- file(filename)
file_contents <- readLines(con, encoding="UTF-8")
close(con)
length(file_contents)

word_indices  = processEmail(file_contents)
x             = emailFeatures(word_indices)
p = svmPredict(model, x)

cat('\nProcessed',filename, '\n\nSpam Classification:', p)
cat('(1 indicates spam, 0 indicates not spam)\n\n')

