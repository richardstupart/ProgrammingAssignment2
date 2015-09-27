## There are two functions. makeCacheMatrix creates a cacheable matrix in much the same way as the 
## demo code, with the change that this supports setInverse()/getInverse() functions instead, used
## to save/retrieve the inverse of the matrix - calculated using the ginv() function in the MASS
## library.

## The cahceSolve function is modelled on the demo code as well, returning the cached copy of the 
## matrix's inverse if one exists, and calculating and storing it otherwise.

## Load the MASS library, so that we can use the ginv() function it provides to calculate our matrix inverses
library("MASS")

## This function creates a list of functions, including functions to get/set the matrix to invert and
## functions getInverse() and setInverse to calculate the inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(inverted) m <<- inverted
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function will solve the supplied matrix. Where a cached inverse is present (!is.null(m)), then
## this will be returned. If none is already cached, it will be computed using the generalised
## matrix inverse function ginv().

cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- ginv(data, ...)
    x$setInverse(m)
    m
}
