## makeCacheMatrix creates a matrix and caches it
## cacheSolve passes in cached matrix and uses solve() function to get inverse value


## Creates functions for getting and setting matrix and inverse matrix values
## and stores values outside of function call

makeCacheMatrix <- function(x = matrix()) {
    inverse_cache <- NULL
    set <- function(y = matrix()) {
        x <<- y
        inverse_cache <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inverse_cache <<- inverse
    getinverse <- function() inverse_cache
    list(set = set, get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
}


##Checks if inverse_cache value is null
##If value is not null, returns value
##If value is null, calls get function and uses the solve function to get the inverse of the matrix
##Then assigns value using setinverse function and returns value

cacheSolve <- function(x, ...) {
        inverse_cache <- x$getinverse()
        if(!is.null(inverse_cache)) {
            message("getting cached data")
            return(inverse_cache)
        }
        data <- x$get()
        inverse_cache <- solve(data, ...)
        x$setinverse(inverse_cache)
        inverse_cache
        
}
