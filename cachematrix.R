## Functions that cache the inverse of a matrix

## First function to create a "matrix" that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(x) {
    mtrx <<- x;
    inverse <<- NULL;
  }
  get <- function() return(mtrx);
  setinv <- function(inv) inverse <<- inv;
  getinv <- function() return(inverse);
  return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}


## Second function to Solve and print matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        
  inverse <- mtrx$getinv()
  if(!is.null(inverse)) {
    message("Getting cached data...")
    return(inverse)
  }
  cdata <- mtrx$get()
  inverse <- solve(cdata, ...)
  mtrx$setinv(inverse)
  return(inverse)
}
