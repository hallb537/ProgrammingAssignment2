## This file should create cached values for inverted matrices,
## allowing the user faster processing times if inversions have
## already been performed.

## The following function creates the cached (inverted) matrix.

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) s <<- solve
        getInverse <- function() s
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## The following function checks inputs against the cached matrix
## to either return cached values or compute new ones.

cacheSolve <- function(x, ...) {
        s <- x$getInverse()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setInverse(s)
        s
}
