## Homework of Cousera_rProgramming Assignment 2
## inverse of a matrix by using caching and lexical scoping

## function makeCacheMatrix creates and stores the inverse of the matrix first

makeCacheMatrix <- function(x = matrix()) {
    inv_x <- NULL
    ## try to filter out the matrix which are not square
    if (nrow(x)!=ncol(x)){
        simpleError(print("matrix given has to be square"))
    }
    else {
        ## here to start the real business
        set <- function(y) {
            x <<- y
            inv_x <<- NULL
        }
        get <- function() x
        setinv <- function(inv) inv_x <<- inv
        getinv <- function() inv_x
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
    }
}

## function cacheSolve will check the value availability within the cache

cacheSolve <- function(x, ...) {
    inv_x <- x$getinv()
    if(!is.null(inv_x)) {
        message("getting cached data")
        return(inv_x)
    }
    data <- x$get()
    inv_x <- solve(data, ...)
    x$setinv(inv_x)
    inv_x
}
