## These two functions are used to compute nd cache INverse of  matrix.Helps prevent 
## recomputing big matrix providing the facility to cache the inverse of a matrix 

## This function creates a special matrix object that cache its own inverse.

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve<- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## This Function computes the inverse of a special matrix returned by makeCacheMatrix.
## If the inverse has already been calculated (matrix doesnt change) 
## then it returns the inverse from the cache

cacheSolve <- function(x, ...) {
           ## Return a matrix that is the inverse of 'x'
           s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s

}
