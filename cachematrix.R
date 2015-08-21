## makeCacheMatrix: This function creates a special "matrix" object that 
## can cache its inverse.
## 
## This object is really a list containing functions to:
## - set the value of the matrix
## - get the value of the matrix
## - set the value of the inverse
## - get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated and the matrix has not changed, then the cachesolve retrieves
## the inverse from the cache.
##
## This function assumes that the matrix is invertible
cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if (!is.null(i)) {
    message("Getting cached inverse matrix")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
