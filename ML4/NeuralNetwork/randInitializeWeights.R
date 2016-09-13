randInitializeWeights <- function(L_in, L_out){
  
  #   W = RANDINITIALIZEWEIGHTS(L_in, L_out) randomly initializes the weights 
  #   of a layer with L_in incoming connections and L_out outgoing 
  #   connections. 
  
  #   Note that W should be set to a matrix of size(L_out, 1 + L_in) as
  #   the column row of W handles the "bias" terms 
  
  
  W = matrix(rep(0,L_out*(1 + L_in)),L_out)
  
  epsilon_init = 0.12
  
  W <- matrix(runif(L_out*(1 + L_in)),L_out)*2*epsilon_init - epsilon_init
  
  W
  
}