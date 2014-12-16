## This combination of functions calculates the
## inverse of a matrix and caches it so that it
## does not need to be recomputed. makeCacheMatrix(x)
## turn a matrix x into a special matrix whose
## inverse can be calculated and cached by passing
## it as an argument to cacheSolve
##
## Example
## Input:
## YourCacheMatrix <- makeCacheMatrix(YourMatrix)
## cacheSolve(YourCacheMatrix)
## Output:
## Inverse of YourMatrix

## makeCacheMatrix takes a matrix as an argument
## and returns a list of functions which can be
## used to set/get the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## cacheSolve calculates the inverse of a matrix
## that has already been passed to makeCacheMatrix.
## If the inverse of the matrix has already been
## calculated, it is retrieved from the cache
## instead of recalculating it.

cacheSolve <- function(x, ...) {
    m <- x$getsolve()
    if(!is.null(m)) {
        message("Retrieving cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
    
}
