## Functions to support and optimise matrix inversion
## by caching the inverse of a matrix rather than compute it repeatedly

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix())
{
  ## Initialise variable m (to store inverse matrix)
  m <- NULL
  
  ## Function to set/initialise a matrix (both the initial and inverse)
  setmatrix <- function(y = matrix()) {
    x <<- y
    m <<- NULL
  }
  
  ## Function to get/return a matrix
  getmatrix <- function() x
  
  ## Function to inverse/solve a matrix
  setinverse <- function(solve) m <<- solve
  
  ## Function to get/return the inverse matrix
  getinverse <- function() m
  
  ## Create list for 4 matrix functions
  list(	setmatrix = setmatrix,
        getmatrix = getmatrix,
        setinverse = setinverse,
        getinverse = getinverse)
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
cacheSolve <- function (x = matrix(), ...)
{
  ## Initialise variables m from cache if it has previously been calculated
  m <- x$getinverse()
  
  ## If variable m don't have a value, then calculate it
  if(!is.null(m))
  {
    message("getting cached data")
    return(m)
  }
  
  ## Store value of input matrix by calling get function 
  data <- x$getmatrix()
  
  ## Calculate inverse of input matrix stored in variable data
  m <- solve(data, ...)
  
  ## Cache result of inverse matrix by calling set function
  x$setinverse(m)
  
  ## Return inverse result
  m
}
