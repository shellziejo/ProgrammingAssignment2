## The following functions cache and compute the inverse of a matrix.
## This function creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() return(x)
  setInv <- function(inverse) m <<- inverse
  getInv <- function() return(m)
  return(list(set = set, get = get, setInv = setInv, getInv = getInv))
}

## This function computes the inverse of the matrix returned by the makeCacheMatrix() above. 
## If the inverse has already been calculated (and the matrix has not changed), then
## cacheSolve() should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getInv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ##get the matrix data
  data <- x$get()
  m <- solve(data, ...)
  
  ##set the invert function on the matrix
  x$setInv(m)
  return(m)
}