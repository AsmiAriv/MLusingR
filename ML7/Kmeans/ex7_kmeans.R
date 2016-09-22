#This file uses the following functions

# To run this script please ensure to store these functions in 
# the same folder as this file and set this folder as working directory

#     computeCentroids.R (For computing centroids)
#     findClosestCentroids.R (For finding the closest centroids)
#     kMeansInitCentroids.R
#     plotDataPoints.R (For plotting data points)
#     plotProgresskMeans.R (Graphically displaying the progression of K-Means)
#     drawLine.R (For drawing lines bewteen moving centroids)
#     runkMeans.R (For running the K-Means)
#     kMeansInitCentroids.R (For random initialization of centroids)
#     pca.R
#     projectData.R
#     recoverData.R




#Setting the working directory
setwd("C:/R/MachineLearning/ML7/Kmeans")

#Initializing
rm(list=ls()) #Using this code will remove all the objects from R session

#loading computeCentroids.R
source('computeCentroids.R')

#loading computeCentroids.R
source('findClosestCentroids.R')

#loading plotDataPoints.R
source('plotDataPoints.R')

#loading plotProgresskMeans.R
source('plotProgresskMeans.R')

#loading drawLine.R
source('drawLine.R')

#loading runkMeans.R
source('runkMeans.R')

#loading kMeansInitCentroids.R
source('kMeansInitCentroids.R')


library(R.matlab) #Required for reading .mat file

# Load an example dataset that we will be using
data <- readMat('ex7data2.mat') 
X <- data[[1]]

#Select an initial set of centroids
K = 3; # 3 Centroids
initial_centroids = matrix(c(3, 3, 6, 2, 8, 5),3, byrow = T)

#Find the closest centroids for the examples using the
# initial_centroids

idx = findClosestCentroids(X, initial_centroids)

cat('Closest centroids for the first 3 examples: \n')
idx[1:3]
cat('\n(the closest centroids should be 1, 3, 2 respectively)\n')

#Compute Means
#  After implementing the closest centroids function, you should now
#  complete the computeCentroids function.

#  Compute means based on the closest centroids found in the previous part.
centroids = computeCentroids(X, idx, K)

cat('Centroids computed after initial finding of closest centroids: \n')
centroids
cat('\n(the centroids should be\n')
cat('   [ 2.428301 3.157924 ]\n')
cat('   [ 5.813503 2.633656 ]\n')
cat('   [ 7.119387 3.616684 ]\n\n')


#K-Means Clustering

#Load an example dataset
data <- readMat('ex7data2.mat')

X <- data[[1]]

#Settings for running K-Means
K = 3
max_iters = 10
initial_centroids = matrix(c(3, 3, 6, 2, 8, 5),3, byrow = T)

#Run K-Means algorithm. The 'true' at the end tells our function to plot
# the progress of K-Means
km <- runkMeans(X, initial_centroids, max_iters, TRUE) 


#K-Means Clustering on Pixels

#  Here we will use K-Means to compress an image. To do this,
#  we will first run K-Means on the colors of the pixels in the image and
#  then map each pixel on to it's closest centroid.

#To read .png file we can use png package and use readPNG function
#To read .jpeg file we can use jpeg package and use readJPEG function
#Our file is png and hence we shall use png and readPNG
library(png)
A <- readPNG("bird_small.png")

#Dimension of the image
img_size = dim(A)

# Reshape the image into an Nx3 matrix where N = number of pixels.
# Each row will contain the Red, Green and Blue pixel values
# This gives us our dataset matrix X that we will use K-Means on.

X = A

dim(X) <- c(img_size[1]*img_size[2], 3)

# Run your K-Means algorithm on this data
# We can try different values of K and max_iters here
K = 16 
max_iters = 10

# When using K-Means, it is important to initialize the centroids randomly. 
# We will use kMeansInitCentroids.m for random initialization of centroids

initial_centroids = kMeansInitCentroids(X, K)

# Run K-Means
km = runkMeans(X, initial_centroids, max_iters)
centroids = km$centroids

#Image Compression
#Now, we'll compress the image using K-Means clusters

#Find closest cluster members
idx = findClosestCentroids(X, centroids)

# Essentially, now we have represented the image X in terms of the
# indices in idx. 

# We can now recover the image from the indices (idx) by mapping each pixel
# (specified by its index in idx) to the centroid value

X_recovered = centroids[idx,]

# Reshape the recovered image into proper dimensions
dim(X_recovered) = c(img_size[1], img_size[2], 3)

# Display the original image 
par(mfrow=c(1, 2))
plot(0, type='n', xlim=0:1, ylim=0:1, main = "Original 128X128 Image")
rasterImage(as.raster(A), 0, 0, 1, 1) 

# Display the Compressed image 
plot(0, type='n', xlim=0:1, ylim=0:1, main = paste0("Compressed with ",K," colors"))
rasterImage(as.raster(X_recovered), 0, 0, 1, 1)


