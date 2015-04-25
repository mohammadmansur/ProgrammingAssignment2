## This script attempts to implement a clumsy caching-enabled mechanism for
## calculating the inverse of a passed matrix with a view to demonstrate R's
## scoping rules, all the while violating a couple of fundamental programming
## best practices :D

## the makeCacheMatrix() function creates the matrix from the passed data and
## dimensions and then creates setters and getters for the matrix and its 
## inverse

## Please note that the arguments to the makeCacheMatrix allows to pass dims
## as well as data in my implementation. Hence the difference from (x=matrix())

makeCacheMatrix <- function(x, ...) {
  
  #init: create matrix from passed data and dimensions, init inverse to NULL
  x <- matrix(x, ...)
  inverse <- NULL
  
  #getters and setters
  set <- function(y,...) {
    #pass data and dims to create parent environment's matrix & init parent's
    #inverse to NULL
    x <<- matrix(x, ...)
    inverse <<- NULL
  }

  get <- function() x

  setInverse <- function(inverse) inverse <<- inverse

  getInverse <- function() inverse

## Return list of functions
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## The cacheSolve function is what you actually call to calculate the caching-
## enabled inverse of the 'special' matrix created by makeCacheMatrix()
cacheSolve <- function(x, ...) {

  inverse <- x$getInverse()
  
## Return inverse if cached
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
## Else calculate, set and return inverse
  data <- x$get()
  inverse <- solve(data, ...)
  x$setInverse(inverse)
  
## Return a matrix that is the inverse of 'x'
  inverse
}
            