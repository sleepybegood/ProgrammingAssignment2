## Matrix inversion is usually a costly computation and their may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly 
## (there are also alternatives to matrix inversion that we will not discuss here). 
## These are a pair of functions that cache the inverse of a matrix.

## This function  creates a  list containing a function to
## -set the value of the matrix
## -get the value of the matrix
## -set the value of the inverse
## -get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  if (ncol(x)!=nrow(x)) {stop("x is not a square matrix")}
  else {  
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
}


## The following function calculates the inverse of the matrix created with the above function

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
