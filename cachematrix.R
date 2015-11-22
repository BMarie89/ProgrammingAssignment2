#Asignment 2
##These functions are able to cache potentially time-consuming computations.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(mymatrix = matrix()) {
  invertmatrix <- NULL
  
  setnewmatrix <- function(newmatrix) {
    mymatrix <<- newmatrix
    invertmatrix <<- NULL
  }
  
  getmatrix <- function() mymatrix
  
  setinvertmatrix <- function(newinvertmatrix) invertmatrix <<- newinvertmatrix
  
  getinvertmatrix <- function() invertmatrix
  
  
  list(setnewmatrix = setnewmatrix, getmatrix = getmatrix,
       setinvertmatrix = setinvertmatrix,
       getinvertmatrix = getinvertmatrix)
       
}


## This function computes the inverse of the special "matrix" 
##returned by makeCacheMatrix above. If the inverse has already been 
##calculated (and the matrix has not changed), then the cachesolve should 
##retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  invert <- x$getinvertmatrix()
  if(!is.null(invert)) {
    message("getting cached data")
    return(invert)
  }
  data <- x$getmatrix()
  invert <- solve(data, ...)
  x$setinvertmatrix(invert)
  invert
  }
