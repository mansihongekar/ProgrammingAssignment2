## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv_cache <- NULL

    setMatrix <- function(y) {
        x <<- y
        inv_cache <<- NULL
    }

    getMatrix <- function() x

    setInverse <- function(inverse) inv_cache <<- inverse

    getInverse <- function() inv_cache

    list(setMatrix = setMatrix,
         getMatrix = getMatrix,
         setInverse = setInverse,
         getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
    inv_cache <- x$getInverse()
    if (!is.null(inv_cache)) {
        message("getting cached data")
        return(inv_cache)
    }
    mat <- x$getMatrix()
    inv_cache <- solve(mat, ...)
    x$setInverse(inv_cache)
    inv_cache
}
