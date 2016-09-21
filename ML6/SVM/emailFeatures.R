emailFeatures <- function(word_indices){
  #EMAILFEATURES takes in a word_indices vector and produces a feature vector
  #from the word indices
  #   x = EMAILFEATURES(word_indices) takes in a word_indices vector and 
  #   produces a feature vector from the word indices. 
  
  # Total number of words in the dictionary
  n = 1899
  x = rep(0,n)
  
  for(i in 1:length(word_indices)){
  l <- word_indices[i]
  x[l] <- 1
  }
  x
}