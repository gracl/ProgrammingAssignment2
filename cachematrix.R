## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse 
makeCacheMatrix <- function(x = matrix()) {
  # Create matrix object, initialize to NULL
  i <- NULL
  
  # Define functions that to operate on the cache matrix:
  # Set matrix value
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  # Get matrix value
  get <- function() x
  
  # Set inverse value
  setinverse <- function(inverse) i <<- inverse
  # Get inverse value
  getinverse <- function() i
  
  # List containing all functions
  list(set = set, 
       get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
    
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
  # Retrieve inverse i
  i <- x$getinverse()
  
  # If i is not null, print that it was cached and return value
  if(!is.null(i)) {
    message("Retrieving from cache")
    return(i)
  }
  
  # Get the matrix and compute the inverse
  data <- x$get()
  i <- solve(data, ...)
  
  # Cache the inverse for the matrix
  x$setinverse(i)
  
  # Return a matrix that is the inverse of 'x'
  i
}
