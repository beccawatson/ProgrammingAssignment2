## This pair of functions will compute an inverse of a matrix 
## and cache it so it does not have be computed repeatedly

## This function creates a special "matrix" object that can cache its inverse. It will
##  1. set the matrix
##  2.  get the matrix
##  3.  set the inverse
##  4. get the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Computes the inverse of the special "matrix" returned by `makeCacheMatrix` above. 
## If the inverse has already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv = x$getinv()
  
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  else matrix.data = x$get()
  inv = solve(matrix.data, ...)
  
  x$setinv(inv)
  
  return(inv)
}
