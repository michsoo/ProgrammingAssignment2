## The two functions below cache the inverse of a matrix

## The first function (makeCacheMatrix) creates a 'matrix' object that can
## cache its inverse

makeCacheMatrix <- function(x = matrix() ) {
    i = NULL
    set <- function (y) {
        x <<- y
        i = NULL
    }
    
    get <- function () x
    setInverse <- function (inverse) i <<- inverse
    getInverse <- function () i
    list (set = set, get = get,
          setInverse = setInverse, getInverse = getInverse)
    
}


## The second function (cacheSolve) computes the inverse of 'matrix' returned
## from makeCacheMatrix above


cacheSolve <- function(x, ...) {
    i <- x$getInverse()
    if (!is.null(i)) {
        message ("getting cached data")
        return(i)
        
    }
    
    matrix  <- x$get()
    i <- solve(matrix, ...)
    x$setInverse(i)
    i
    
}