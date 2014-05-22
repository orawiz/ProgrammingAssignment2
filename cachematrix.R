## The following functions are designed to remember or cache a 
## previous matrix inverse result.
## An inverse matrix calculation can be expensive to prepare.
## When a previous calculation is available in cache this is provided
## instead of calculating again.

## The makeCacheMatrix function manages the cached matrix inverse result.
## This function provides a way to set and get the inverse result provided by cacheSolve.
## This function references a parent environment to locate the inverse result.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function( invert) inv <<- invert
        getinv <- function() inv
        list( set = set, get = get,
              setinv = setinv,
              getinv = getinv)         
}


## The cacheSolve function calculates a square matrix inverse using the solve function
## When available provides the previous result saved to cache 
## Delegates to makeCacheMatrix to maintain the cache

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
        
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        inv
}
