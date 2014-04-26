## This function creates a special "matrix" object that can cache its inverse.
## The first function is really a list containing a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. cache the inversed matrix produced by the second function
## 4. get the inversed matrix produced by the second function

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setSolve <- function(solve) m <<- solve
    getSolve <- function() m
    list(set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve)
}

## The following second function produces the inversed matrix by "Solve".
## It first checks to see if there exists a previously inversed matrix.
## If so, it gets the previous result and skips the computation.
## Otherwise, it produces the "Solve"d inversed matrix and return.

cacheSolve <- function(x, ...) {
    m <- x$getSolve()               #query the x matrix's cache         
    if(!is.null(m)) {               #if there is a cache
        message("getting cached data") 
        return(m)                    #just return the cache, no computation needed
    }
    data <- x$get()                 #if there's no cache
    m <- solve(data, ...)           #we actually compute them here
    x$setSolve(m)                   #save the result back to x's cache
    m                               #return the result
}
