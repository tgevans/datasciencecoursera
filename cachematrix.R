## Converts a matrix into a specially defined matrix with 
## metadata storing the inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(invrs) inv <<- invrs
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Uses solve to find the inverse; uses cached version if the
## matrix already contains the inverse in metadata

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ## NOTE: assumes that x is invertible
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
