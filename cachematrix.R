## Programming Assignment 2 for Coursera course rprog-015
##
## Implement two functions to eable caching the inverse of a matrix
#  
# Usage:  
#     cm <- makeCacheMatrix()
#     cm$set(matrix(c(1,2,3,4),2,2))
#     im <- cacheSolve(cm)
#     im <- cacheSolve(cm)    # get from cache
#     cm$set(matrix(c(1,2,3,4),2,3))
#     im <- cacheSolve(cm)    # get from cache

## Store a given matrix and provide functions to get and set its inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    if (!identical(y,x)) {
      x <<- y
      m <<- NULL
    }
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Return cached inverse if it exists, otherwise calculate, store and return the inverse 
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
