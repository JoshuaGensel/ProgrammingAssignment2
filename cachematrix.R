# These functions allow to take a matrix and cache its inverse.
# To accomplish this a list is created which stores set and get functions for 
# the matrix object as well as its inverse.
# Then the second function can take a matrix as argument and see if the inverse
# of that matrix is already saved in the cache.
# If it is it just returns the cached inverse.
# Otherwise it will compute the inverse and set it in the cache.

## Create functions to cache matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(matrixInverse) m <<- matrixInverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Cache matrix inverse or calculate and set it

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
