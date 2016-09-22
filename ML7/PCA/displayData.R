displayData <- function(X){
  
  #DISPLAYDATA Display 2D data in a nice grid
  #   [h, display_array] = DISPLAYDATA(X, example_width) displays 2D data
  #   stored in X in a nice grid. It returns the figure handle h and the 
  #   displayed array if requested.
  
  X <- as.matrix(X) #Converting into matrix for matrix operations
  
  # Calculate example_width
  
  example_width = round(sqrt(ncol(X)))
  
  #Compute rows, cols
  m <- nrow(X)
  n <- ncol(X)
  #Compute example_height
  example_height = (n / example_width)
  
  
  #Compute number of items to display
  display_rows = floor(sqrt(m))
  display_cols = ceiling(m / display_rows)
  
  #Between images padding
  
  pad = 1
  
  #Setup blank display
  display_array = matrix(rep(-1,(pad + display_rows*(example_height + pad))*(pad + display_cols*(example_width + pad))),
                        nrow=pad + display_rows * (example_height + pad))
  
  #Copy each example into a patch on the display array
  curr_ex = 1
  for (j in 1:display_rows){
  for (i in 1:display_cols){
  if (curr_ex > m)break 
  
    # Copy the patch
    
    # Get the max value of the patch
    max_val = max(abs(X[curr_ex,]))
    
    X_n <- as.matrix(X[curr_ex,]) #A new vector
    
    dim(X_n) <- c(example_height, example_width) #converting to a matrix
    
    display_array[pad + (j - 1) * (example_height + pad) + (1:example_height),
                  pad + (i - 1) * (example_width + pad) + (1:example_width)] =
    X_n / max_val
    curr_ex = curr_ex + 1
    
          }
    if (curr_ex > m)break
  }
  
#Display Image 
image(t(display_array), axes = FALSE,col  = gray((0:100)/100))
  
}
   
