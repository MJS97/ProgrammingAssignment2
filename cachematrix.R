## These two functions work in conjuction to find, store, and retrieve 
## the inverse of an invertable matrix. We assume x is invertable.

## Inputs: for makeCacheMatrix: x, an invertable matrix object
##         for cacheSolve: x,  a "special" invertable matrix object 
##                         as returned by makeCacheMatrix
## Outputs: makeCacheMatrix returns object of list class with cached values
##          cacheSolve returns the inverse of x as a matrix object

## makeCacheMatrix creates a special matrix object that stores inverse of x
## in an outside environment

library(MASS)

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(ginv) inv <<- ginv
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve returns the inverse of x by first checking whether
## the inverse is already cached, if so returning the stored object
## if not then calculating the inverse, and caching it before returning
## the calculated inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- ginv(data)
  x$setinv(inv)
  inv
}

