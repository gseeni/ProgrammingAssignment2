## Put comments here that give an overall description of what your
## functions do

## This funciton creates the special matrix object that can cache its reverse

makeCacheMatrix <- function(x = matrix()) {
  rev <- NULL
  set <- function(y) {
    x <<- y
    rev <<- NULL
  }
  get <- function() x
  setrev <- function(inverse) inv <<- inverse
  getrev <- function() rev
  list(set = set, get = get, setrev = setrev, getrev = getrev)
}


## This function computes the inverse of the special matrix returned by above function.  
## if this has been already calculated, then it will return from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  rev <- x$getrev()
  if(!is.null(rev)) {
    message("getting cached result")
    return(rev)
  }
  data <- x$get()
  rev <- solve(data, ...)
  x$setrev(rev)
  rev
}