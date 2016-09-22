runkMeans <- function(X, initial_centroids,max_iters, plot_progress=FALSE){
  
  #RUNKMEANS runs the K-Means algorithm on data matrix X, where each row of X
  #is a single example
  #   [centroids, idx] = RUNKMEANS(X, initial_centroids, max_iters, ...
  #   plot_progress) runs the K-Means algorithm on data matrix X, where each 
  #   row of X is a single example. It uses initial_centroids used as the
  #   initial centroids. max_iters specifies the total number of interactions 
  #   of K-Means to execute. plot_progress is a true/false flag that 
  #   indicates if the function should also plot its progress as the 
  #   learning happens. This is set to false by default. runkMeans returns 
  #   centroids, a Kxn matrix of the computed centroids and idx, a m x 1 
  #   vector of centroid assignments (i.e. each entry in range [1..K])
  
  
  # Set default value for plot progress
  #Plot the data if we are plotting progress
 if(plot_progress) {
   plot(X[,1],X[,2],type="n")
   title(main=paste0('Centroids Movement in ', max_iters,' Iterations' ))
 }
   
  
  
  # Initialize values
  m <- nrow(X)
  n <- ncol(X)
  K <- nrow(initial_centroids)
  centroids <- initial_centroids
  previous_centroids <- centroids
  idx <- rep(0,m)
  
  #Run K-means
  for(i in 1:max_iters){
    #Output progress
    cat('K-Means iteration', i,'\n')
    
    #For each example in X, assign it to the closest centroid
    idx = findClosestCentroids(X, centroids)
    
    #Optionally, plot progress here
    if (plot_progress){
     
      plotProgresskMeans(X, centroids, previous_centroids, idx, K, i);
      previous_centroids = centroids
      cat ("Press [enter] to continue")
      line <- readline()
    }
    
    #Given the memberships, compute new centroids
    centroids = computeCentroids(X, idx, K)
    
  }
    
  
  
 list(centroids=centroids,idx=idx) 
 
}