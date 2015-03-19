## The following functions allow to cache the inverse of a matrix so the cached value can be used 
## whenever needed rather than calculating it repeatedly, as it can be a costly operation.
## The function assumes that the matrix supplied is always invertible


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverse_matrix <- NULL 
  y <- NULL 
  set <- function(y) { # set the value of the matrix
    x <<- y  # caches the input matrix so we can check if inverse has already been calculated for the given input
    inverse_matrix <<- NULL # sets the value for the cached value of the inverse matrix
  }
  get <- function() x
  setinverse <- function(solve) inverse_matrix <<- solve
  getinverse <- function() inverse_matrix
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve
## should retrieve the inverse from the cache.

cacheSolve <- function(x=matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'

  inverse_matrix <- x$getinverse()   # Look up x in cache and assign the cached value to inverse_matrix

  if (!is.null(inverse_matrix)){   # Got a hit in cache, so ok to skip calculation
      message("getting cached data")
      return(inverse_matrix)  # return cached value for the inverse matrix
  }
  # If matrix was not found in cache then calculate the inverse
  y <- x$get()
  inverse_matrix <- solve(y, ...)
  
  x$setinverse(inverse_matrix) # cache the calculated value of the inverse
  return(inverse_matrix)
}


