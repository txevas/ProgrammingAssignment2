## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##
## Returns a list of functions. Two pairs of get/set functions
##
## set/setsolve: stores the matrix/inverse's matrix in parent environment
## get/getsolve: retrieves the matrix/inverse's matrix
##

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Write a short comment describing this function
##
## Returns an inverse's matrix, which value can be cached
##
## It calculates the inverses' matrix if there is no previous cached value (not null)
##

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
  
}
