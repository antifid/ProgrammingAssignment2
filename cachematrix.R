## Two functions to help with inverting matrices and keeping a cached result
## in case the matrix needs to be inverted repeatedly.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    # Create or reset the connection to the underlying matrix,
    # with a NULL inverse initially.
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    # Other functions for handling this matrix
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    # Return the functions to access this cache-matrix
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
# above. If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    ## Invert the cache-matrix
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    basematrix <- x$get()
    m <- solve(basematrix, ...)
    x$setinverse(m)
    m
}