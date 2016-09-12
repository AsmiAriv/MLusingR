predict <- function(theta, X){
  
#PREDICT Predict whether the label is 0 or 1 using learned logistic 
#regression parameters theta
#p = PREDICT(theta, X) computes the predictions for X using a 
#threshold at 0.5 (i.e., if sigmoid(theta'*x) >= 0.5, predict 1)
  
  m = nrow(X) #Number of training examples

#You need to return the following variables correctly
  
p = rep(0,m)

for (j in 1:m){
if((sigmoid(X%*%theta))[j] >= 0.5)
p[j] = 1
else
  p[j] = 0
  
 }  
  p
}