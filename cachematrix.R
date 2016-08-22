## Caching the Inverse of a Matrix:
## First Function 
makeCacheMatrix <- function(M = matrix()) {
  I <- NULL
  set <- function(m) {
    M <<- m
    I <<- NULL
  }
  get <- function() M
  setInverse <- function(inverse) I <<- inverse
  getInverse <- function() I
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" created by first function

cacheSolve <- function(M, ...) {
  ## Return a matrix that is the inverse of 'M'
  I <- M$getInverse()
  if (!is.null(I)) {
    message("getting cached data")
    return(I)
  }
  mat <- M$get()
  I <- solve(mat, ...)
  M$setInverse(I)
  I
}