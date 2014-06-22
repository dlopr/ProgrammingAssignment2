## cachematrix.R
## This script provides functions to support chaching the inverse of a matrix

## makeCacheMatrix
## Returns a list of functions that set/get a matrix value and set/get
## its inverse matrix with the input argument 'x' as the initial value.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve
## Return a matrix that is the inverse of 'x'. The argument 'x' should have
## been constructed with makeCacheMatrix

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}
