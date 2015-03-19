## makeCacheMatrix will form the matrix to be cached
## cacheSolve will invert the matrix

makeCacheMatrix <- function(x = matrix()) {
     
     invMatrix <- NULL ##Initialize the local invMatrix as NULL
     set <- function (y) { ##Initiate function to set cached matrix
          x <<- y ##Set the value of x in the parent env
          invMatrix <<- NULL ##Set the invMatrix to NULL in the parent env
          }
     get <- function() x ##Return the matrix x in the evaluating env
     setInv <- function(inv) invMatrix <<- inv ##Set invMatrix in parent env to the inverse
     getInv <- function() invMatrix ##Return the invMatrix in the evaluating env
     list(set = set, get = get, 
          setInv = setInv,
          getInv = getInv)
}


cacheSolve <- function(x, ...) {
     invMatrix <- x$getInv() #Get the inverse stored in the 'x' environment
     if(!is.null(invMatrix)){ ##If the inverse was previously calculated
          message("getting cached data")
          return(invMatrix) ##Return invMatrix (containing the inversed matrix)
     }
     data <- x$get() ##If this matrix has not been inverted before
                     ##Get the matrix and store as 'data'
     invMatrix <- solve(data, ...)
     x$setInv(invMatrix) ##Assign the computed inverse to the environment of 'x'
     
     invMatrix #Display the inverted matrix
}
