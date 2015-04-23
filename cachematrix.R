##  Overall description of Caching the Inverse of a Matrix function
#
#  This function is for improving execution performance 
#  by caching computed values. 
#  Matrix inversion is a CPU intensive function 
#  by caching the inverse of a matrix the performance is improved.
#  Caching is done by using the <<- operator which assign a value to an object in an 
#  environment that is different from the current environment.
#  Functions:
#  1.makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
#  2.cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix
#  If the inverse has already been calculated (and the matrix has not changed), then the 
#  cachesolve function retrieves the inverse from the cache.

##
## Assumtion for this is the matrix supplied is always invertible
##

## Write a short comment describing this function
#  The makeCacheMatrix function, creates a special "matrix" object which is a list containing 
#   a function to 
#   1.set the value of the matrix
#   2.get the value of the matrix
#   3.set the value of the solve (inverse)
#   4.get the value of the solve (inverse)
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL 
  set <- function(y) { 
    x <<- y 
    m <<- NULL 
  } 
  get <- function() x 
  setsolve <- function(solve) m <<- solve 
  getsolve <- function() m 
  list(set = set, get = get, 
       setsolve = setsolve, 
       getsolve = getsolve)
}


## Write a short comment describing this function
# cacheSolve function
#  This function computes the inverse of the special "matrix" returned by above makeCacheMatrix function
#  It first checks to see if the solve(inverse) has already been calculated.
#  IF so, it gets the solve(inverse) from the cache and skip the computation.
#  Otherwise it computes and sets the value in cache using setsolve function.
#  This function return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x' 
  m <- x$getsolve() 
  if(!is.null(m)) { 
    message("getting cached data") 
    return(m) 
  } 
  data <- x$get() 
  m <- solve(data, ...) 
  x$setsolve(m) 
  m
}
