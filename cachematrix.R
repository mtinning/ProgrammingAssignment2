## These functions allow you to create a matrix which is able
## to cache the result of the solve() function.

## This function returns an object that represents a cached matrix.
## It has a number of functions:
## - set(x) sets the matrix stored
## - get() returns the matrix stored by this object
## - setinverse(i) caches the inverse of the matrix as i
## - getinverse() gets the cached inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(i) inverse <<- i
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Solves the matrix, returning the cached inverse if possible

cacheSolve <- function(x, ...) {
        if (length(list(...)) > 0) {
                message("Caching not supported for additional args; returning solved matrix")
                return(solve(x$get(), ...))
        }
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if (!is.null(i)) {
                message("Getting cached inverse")
                return (i)
        }
        i <- solve(x$get(), ...)
        x$setinverse(i)
        i
}
