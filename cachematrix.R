## This function creates a special "matrix" object that can cache its inverse.
## It has 4 functions to set, get the value of the matrix
## and set get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  ## clear cache and create set/get functions
  inv <- NULL
  ## set the inverse
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(xinv) inv <<- xinv
  getinv <- function() inv
  ## return a list with functions
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}
## This function calculates the inverse of the
## "special matrix". It first checks if the inverse
## had already been calculated. If so it gehts the
## inverse from the cache an skips the calculation.
## Otherwise the inverse is calculated and set into
## the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  ## checks if already calculated
  if (!is.null(inv)) {
    message("getting cached inverse matrix")
    return(inv)
  }
  ## if not get the matrix and calculate the inverse
  message("calculating inverse matrix")
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinv(inv)
  inv
}