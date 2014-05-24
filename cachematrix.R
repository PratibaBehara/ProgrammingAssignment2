## This file contains functions which calculates and caches the inverse of a matrix.

## makeCacheMatrix function sets and returns the given matrix and its inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  ##binds the 'x' value to new matrix and 'm' to null everytime makecacheMatrix is called 
  set <- function(y) {
    x <<- y
    m <<- NULL
    
  } 
  ## returns the matrix 
  get <- function() x
  ##bind the 'm' to inverse of matrix 
  setinverse <- function(inverse) m <<- inverse
  ##returns the inverse matrix
  getinverse <- function() m
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSOlve function which takes the output from make makeCacheMatrix.
##Returns the inverse of a matrix,if already calculated the cached value.

cacheSolve <- function(x, ...) {
        ##check whether cachedValue is present
  m <- x$getinverse()
  ## if present, get it
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
        ##else calculate the inverse of matrix
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
