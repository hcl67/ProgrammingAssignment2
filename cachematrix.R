## these functions cache the inverse of matrix, and if possible, use the cache rather than calculate the inverse 
## every time

## function to store the matrix and its inversion
## Args:
##   x: the matrix to calculate the inverse
## Return:
##   set: function to set the value of matrix x
##   get: function to get the value of matrix x
##   setinv: function to set the value of inverse matrix of x
##   getinv: function to get the value of inverse matrix of x

makeCacheMatrix <- function(x = matrix()) {
  invx <- NULL
  set <- function(y) {
    x <<- y
    invx <<- NULL
  }
  get <- function() x
  setinv <- function(inv) invx <<- inv
  getinv <- function() invx
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## function to get and cache the inverse matrix and the cached value would be used if possible
## Args:
##   x: the matrix to calculate the inverse
## Return:
##   invx: inverse matrix of x

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  m <- x$get()
  invx <- solve(m, ...)
  x$setinv(invx)
  invx
}
