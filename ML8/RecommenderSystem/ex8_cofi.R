#This file uses the following functions

# To run this script please ensure to store these functions in 
# the same folder as this file and set this folder as working directory

#     computeNumericalGradient.R (For computing numerical gradient)
#     checkCostFunction.R (For checking the cost function)
#     normalizeRatings.R (For normalizing ratings)
#     cofiCostFunc.R (For using optimizer in R to optimize theta at minimized cost)
#     cofiGradFunc.R (For using optimizer in R to optimize theta at minimized cost)

#Setting the working directory
setwd("C:/R/MachineLearning/ML8/RecommenderSystem")

#Initializing
rm(list=ls()) #Using this code will remove all the objects from R session


#Loading computeNumericalGradient function
source('computeNumericalGradient.R')

#Loading checkCostFunction function
source('checkCostFunction.R')

#Loading normalizeRatings function
source('normalizeRatings.R')

#Loading cofiCostFunc.R function
source('cofiCostFunc.R')

#Loading linearReggradFunction function
source('cofiGradFunc.R')


library(R.matlab) #Required for reading .mat file

# Load Training Data
data <- readMat('ex8_movies.mat') # training data stored in arrays Y and R

#Train set
Y <- data[[1]]
R <- data[[2]]

#Y is a 1682x943 matrix, containing ratings (1-5) of 1682 movies on 
#  943 users

#  R is a 1682x943 matrix, where R(i,j) = 1 if and only if user j gave a
#  rating to movie i

#  From the matrix, we can compute statistics like average rating.

cat('Average rating for movie 1 (Toy Story): \n\n',mean(Y[1, which(R[1,]==1)]),'/5')

#We can "visualize" the ratings matrix by plotting it with image
image(t(Y))

#Collaborative Filtering Cost Function
#Load pre-trained weights (X, Theta, num_users, num_movies, num_features)
data_params <- readMat('ex8_movieParams.mat')
X <- data_params$X
Theta <- data_params$Theta

#Reduce the data set size so that this runs faster
num_users = 4; num_movies = 5; num_features = 3;
X = X[1:num_movies, 1:num_features]
Theta = Theta[1:num_users, 1:num_features]
Y = Y[1:num_movies, 1:num_users]
R = R[1:num_movies, 1:num_users]

#Evaluate cost function
J = cofiCostFunc(c(as.vector(X), as.vector(Theta)), Y, R, num_users, num_movies,
                 num_features, 0)

cat('Cost at loaded parameters:',J,'\n(this value should be about 22.22)')

#Collaborative Filtering Gradient
#Check gradients by running checkCostFunction

checkCostFunction()

#Collaborative Filtering Cost Regularization
#Evaluate cost function
J = cofiCostFunc(c(as.vector(X), as.vector(Theta)), Y, R, num_users, num_movies,
                 num_features, 1.5)
cat('Cost at loaded parameters (lambda=1.5):',J,'\n(this value should be about 31.34)')

#Collaborative Filtering Gradient Regularization
#Check gradients by running checkNNGradients
checkCostFunction(1.5)

#Entering ratings for a new user
#Before we will train the collaborative filtering model, we will first
#  add ratings that correspond to a new user that we just observed. This
#  part of the code will also allow you to put in your own ratings for the
#  movies in our dataset!

#Load list of movies
con <- file('movie_ids.txt')
movieList <- readLines(con)
close(con)
length(movieList)

#Removing the numbers attached with each movie
for(i in 1:length(movieList)){
  movieList[i] <- gsub("^[[:digit:]]*\\ *","",movieList[i])
   }

#Initialize my ratings
my_ratings = rep(0,1682)

#Check the file movie_idx.txt for id of each movie in our dataset
# For example, Toy Story (1995) has ID 1, so to rate it "4", you can set
my_ratings[1] = 4

#Or suppose did not enjoy Silence of the Lambs (1991), you can set
my_ratings[98] = 2

# We have selected a few movies we liked / did not like and the ratings we
# gave are as follows:
my_ratings[7] = 3
my_ratings[12]= 5
my_ratings[54] = 4
my_ratings[64]= 5
my_ratings[66]= 3
my_ratings[69] = 5
my_ratings[183] = 4
my_ratings[226] = 5
my_ratings[355]= 5

cat('\n\nNew user ratings:\n')
for(i in 1:length(my_ratings)){
  if (my_ratings[i] > 0){
    cat('Rated',my_ratings[i], 'for', movieList[i],'\n')
  }
}


#Learning Movie Ratings
# Now, we will train the collaborative filtering model on a movie rating 
#  dataset of 1682 movies and 943 users
data <- readMat('ex8_movies.mat') # training data stored in arrays Y and R

#Train set
Y <- data[[1]]
R <- data[[2]]

Y <- cbind(my_ratings,Y)
R <- cbind(1*(my_ratings!=0),R)

# Normalize Ratings
norm <- normalizeRatings(Y, R)

Ynorm <- norm$Ynorm
Ymean <- norm$Ymean

#Useful Values
num_users = ncol(Y)
num_movies = nrow(Y)
num_features = 10

#Set Initial Parameters (Theta, X)
X = matrix(rnorm(num_movies*num_features),num_movies)
Theta = matrix(rnorm(num_users*num_features),num_users)

initial_parameters = c(as.vector(X), as.vector(Theta))

#Set Regularization
lambda = 10

costh <- optim(par=initial_parameters, fn=cofiCostFunc, gr=cofiGradFunc, 
               method="BFGS", Y, R, num_users, num_movies,num_features, lambda,
               control = list(maxit=100))

params <- costh$par
cost <- costh$value

# Unfold the U and W matrices from params
X <- params[1:(num_movies*num_features)]
dim(X) <- c(num_movies, num_features)

Theta <- params[(num_movies*num_features+1):length(params)]
dim(Theta) <- c(num_users, num_features)

#Recommendation for you
#After training the model, you can now make recommendations by computing
#  the predictions matrix.

p = X %*% t(Theta)
my_predictions = p[,1] + Ymean

ix <- order(my_predictions,decreasing=TRUE)

cat("Top recommendations")
for(i in 1:10){
  j <- ix[i]
  cat('Predicted Rating',my_predictions[j], 'for', movieList[j],'\n')
  
}

cat('\n\nOriginal ratings provided:\n')
for(i in 1:length(my_ratings)){
  if (my_ratings[i] > 0){
    cat('Rated',my_ratings[i], 'for', movieList[i],'\n')
  }
}













