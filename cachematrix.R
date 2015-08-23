## These functions allow to cache the inverse of a matrix
## to avoid expensive redundant computations

## This methods wraps an existing matrix into a new object
## which provides a cache for the inverse of the matrix
## set(y):      sets the matrix to y
## get():       returns the current matrix
## getsolve():  returns the cached matrix inverse if available;
##              otherwise computes the inverse, caches the result and returns it
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  getsolve <- function() {
    ## keeping the computation inside the cache
    ## so as to encapsulate the cache
    if (is.null(inv)) {
      inv <<- solve(x)
    }
    inv
  }
  list(set=set, get=get, getsolve = getsolve)
}


## Returns the inverse of a wrapped matrix, potentially from cache
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  x$getsolve()
}