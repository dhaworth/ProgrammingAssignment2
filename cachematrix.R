## These functions allow for performing a matrix inversion, then caching the result
## for reuse later

## This function acts as the caching environment for inverted matrices, allowing for
## setting and getting the values stored within

makeCacheMatrix <- function(x = matrix()) {
    mi <- NULL
    set <- function(y) {
        x <<- y
        mi <<- NULL
    }
    get <- function() x
    setMatrixInverse <- function(matrixInverse) mi <<- matrixInverse
    getMatrixInverse <- function() mi
    list(set = set, get = get,
         setMatrixInverse = setMatrixInverse,
         getMatrixInverse = getMatrixInverse)
}

## This function will return a cached inverted matrix if it exists or calculate 
## it, cache it, then return it

cacheSolve <- function(x, ...) {
    mi <- x$getMatrixInverse()
    if (is.null(mi)) {
        data <- x$get()
        mi <- solve(data, ...)
        x$setMatrixInverse(mi)
    }
    else
    {
        message("getting cached data")
    }
    
    mi
}
