## The two functions below are used for creating and storing a matrix object
## as well as caching its inverse.

## Creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  data_inv <- NULL
  set <- function(y) {
    x <<- y
    data_inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) data_inv <<- inverse
  getInverse <- function() data_inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Computes the inverse of the matrix created by makeCacheMatrix
## If the inverse has been generated, and the matrix object has not
## changed, then the inverse matrix should be fetched from cache.


cacheSolve <- function(x, ...) {
  ## Returns a matrix that is the inverse of 'x'
  data_inv <- x$getInverse()
  if (!is.null(data_inv)) {
    message("fetching cached data")
    return(data_inv)
  }
  data <- x$get()
  data_inv <- solve(data, ...)
  x$setInverse(data_inv)
  data_inv
}
