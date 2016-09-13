computeNumericalGradient <- function(J1, theta){
  
  #COMPUTENUMERICALGRADIENT Computes the gradient using "finite differences"
  #and gives us a numerical estimate of the gradient.
  #   numgrad = COMPUTENUMERICALGRADIENT(J, theta) computes the numerical
  #   gradient of the function J around theta. Calling y = J(theta) should
  #   return the function value at theta.
  
  
  numgrad = rep(0,length(theta))
  
  perturb = rep(0,length(theta))
  e = 1e-4
  for (p in 1:length(theta)){
  # Set perturbation vector
  perturb[p] = e
  loss1 = J1(theta - perturb)
  loss2 = J1(theta + perturb)
  # Compute Numerical Gradient
  numgrad[p] = (loss2$J - loss1$J) / (2*e)
  perturb[p] = 0
  
    }
  numgrad
  
}