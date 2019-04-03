## This pair of functions allows us to cache the inverse of a matrix,
## instead of computing it repeatedly whenever it is needed.

## Creates a matrix that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  x.inv <- NULL
  set <- function(y) {
    x <<- y
    x.inv <<- NULL
  }
  get <- function() x
  setinv <- function(xi) {
    x.inv <<- xi
  }
  getinv <- function() x.inv
  
  list(set=set, get=get, 
       setinv=setinv, getinv=getinv)
}

## Returns the cached inverse for the given matrix, if available;
## otherwise, solves for (and caches) the inverse.
cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if (!is.null(i)) {
    message("Getting cached inverse value")
    return(i)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
