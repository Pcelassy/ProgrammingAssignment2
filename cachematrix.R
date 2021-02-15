## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## A pair of functions that cache the inverse of a matrix
## Create matrix object that can cache its inverse
makeCacheMatrix <- function(m = matrix()) {
  ## Initializing inverse property
  i <- NULL
  
  ## Setting the matrix
  set <- function( matrix ) {
    m <<- matrix
    i <<- NULL
  }
  ## Get matrix function
  get <- function() {
    ## Return matrix
    m
  }
  
  ## Setting inverse of matrix
  setInverse <- function(inverse) {
    i <<- inverse
  }
  
  ## Get inverse of  matrix
  getInverse <- function() {
    ## Return inverse 
    i
  }
  
  ## Return list of methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  
  
  if( !is.null(m) ) {
    message("fetching cached data")
    return(m)
  }
  
  ## Get matrix from object
  data <- x$get()
  
  ## Calculate inverse using matrix multiplication
  m <- solve(data) %*% data
  
  ## Set inverse to object
  x$setInverse(m)
  
  ## Return matrix
  m
}
