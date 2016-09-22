#This file uses the following functions

# To run this script please ensure to store these functions in 
# the same folder as this file and set this folder as working directory

#     pca.R (For calculating S and U)
#     projectData.R (For projecting data on K dimension)
#     recoverData.R (For recovering an approx of original data)
#     displayData.R (For displaying data)
#     featureNormalize.R (For normalizing features)
#     drawLine.R (for drawing the lines)




#Setting the working directory
setwd("C:/R/MachineLearning/ML7/PCA")

#Initializing
rm(list=ls()) #Using this code will remove all the objects from R session

#loading pca.R
source('pca.R')

#loading displayData.R
source('displayData.R')

#loading projectData.R
source('projectData.R')

#loading recoverData.R
source('recoverData.R')

#loading featureNormalize.R
source('featureNormalize.R')

#loading drawLine.R
source('drawLine.R')


library(R.matlab) #Required for reading .mat file

# Load an example dataset that we will be using
data <- readMat('ex7data1.mat') 

X <- data[[1]]

#Visualize the example dataset
plot(X[, 1], X[, 2], col='blue', pch=1,
     xlim=c(0.5, 6.5), ylim=c(2, 8))

#Principal Component Analysis

#Before running PCA, it is important to first normalize X

fn <- featureNormalize(X)

X_norm <- fn$X
mu <- fn$mu
sigma <- fn$sigma

#Run PCA
pc <- pca(X_norm)

U <- pc$U
S <- pc$S

#Compute mu, the mean of the each feature

#  Draw the eigenvectors centered at mean of data. These lines show the
#  directions of maximum variations in the dataset.

drawLine(mu, mu + 1.5 * S[1,1] %*% t(U[,1]), col='black', lwd=2)
drawLine(mu, mu + 1.5 * S[2,2] %*% t(U[,2]), col='black', lwd=2)

cat('Top eigenvector: \n')
cat(U[1,1], U[2,1])
cat('\n(you should expect to see -0.707107 -0.707107)\n')

#Dimension Reduction
#  We should now implement the projection step to map the data onto the 
#  first k eigenvectors. The code will then plot the data in this reduced 
#  dimensional space.  This will show what the data looks like when 
#  using only the corresponding eigenvectors to reconstruct it.

#  We will use projectData.R

#Plot the normalized dataset (returned from pca)
plot(X_norm[, 1], X_norm[, 2], col='blue', pch=1,
     xlim=c(-4, 3), ylim=c(-4, 3))

#Project the data onto K = 1 dimension
K = 1
Z = projectData(X_norm, U, K)

cat('Projection of the first example:', Z[1])
cat('\n(this value should be about 1.481274)\n\n')

X_rec  = recoverData(Z, U, K)
cat('Approximation of the first example:', X_rec[1, 1], X_rec[1, 2])
cat('\n(this value should be about  -1.047419 -1.047419)\n\n')

#Draw lines connecting the projected points to the original points
points(X_rec[, 1], X_rec[, 2], col='red', pch=1,
     xlim=c(-4, 3), ylim=c(-4, 3))

for(i in 1:nrow(X_norm)){
  
  drawLine(X_norm[i, ], X_rec[i, ], col='black', lwd=1,lty=2)
  
}

#Loading and Visualizing Face Data
data <- readMat('ex7faces.mat') 
X <- data[[1]]

#Display the first 100 faces in the dataset
displayData(X[1:100, ])

#PCA on Face Data: Eigenfaces
#Run PCA and visualize the eigenvectors which are in this case eigenfaces
# We display the first 36 eigenfaces.

#Normalize the features
fn <- featureNormalize(X)

X_norm <- fn$X
mu <- fn$mu
sigma <- fn$sigma

#Run PCA
pc <- pca(X_norm)

U <- pc$U
S <- pc$S

#  Visualize the top 36 eigenvectors found
displayData(t(U[, 1:36]))

#Dimension Reduction for Faces

#Project images to the eigen space using the top k eigenvectors 
#If you are applying a machine learning algorithm

K = 100
Z = projectData(X_norm, U, K)

cat('The projected data Z has a size of: ')
dim(Z)

#Visualization of Faces after PCA Dimension Reduction
#  Project images to the eigen space using the top K eigen vectors and 
#  visualize only using those K dimensions
#  Compare to the original input, which is also displayed

K = 100
X_rec  = recoverData(Z, U, K)

# Display normalized data
par(mfrow=c(1, 2))
displayData(X_norm[1:100,])
title('Original faces')


# Display reconstructed data from only k eigenfaces
displayData(X_rec[1:100,])
title('Recovered faces')




