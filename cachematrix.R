## Put comments here that give an overall description of what your
## functions do: 
## 1. makeCacheMatrix - This function creates a special "matrix" object that can cache its inverse.
## 2. cacheSolve - This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##                 If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
##                 should retrieve the inverse from the cache.


## 1. makeCacheMatrix creates a special "matrix", which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the Inverse
## get the value of the Inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y 
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

## 2. cacheSolve calculates the Inverse of the special "matrix" created with the above function. 
##      However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache 
##      and skips the computation. Otherwise, it calculates the inverse of the data and sets the value of the inverse in the 
##      cachecvia the setsolve function.

cacheSolve <- function(x, ...) {
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}
