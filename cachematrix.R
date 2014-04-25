## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setSolve <- function(solve) m <<- solve #inverse matrix by solve
    getSolve <- function() m
    list(set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getSolve()           #query the x vector's cache         
    if(!is.null(m)) {           #if there is a cache
        message("getting cached data") 
        return(m)                #just return the cache, no computation needed
    }
    data <- x$get()             #if there's no cache
    m <- solve(data, ...)       #we actually compute them here
    x$setSolve(m)               #save the result back to x's cache
    m                           #return the result
}
