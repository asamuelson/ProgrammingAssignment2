## The functions makeCacheMatrix and cacheSolve calculate the inverse of a 
## square, invertible matrix. They also check whether the inverse has already been
## calculated and use the cached version of the answer if it is available.  
## These functions are based on the sample functions makeVector and cacheMean.

## The function makeCacheMatrix creates a special "matrix" object. 
## It creates a list containing a function which performs the following tasks:
## 1. Set the matrix value
## 2. Get the matrix value
## 3. Set the matrix inverse value
## 4. Get the matrix inverse value
 
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve(x)
    getsolve <- function() m
    list(set = set, get = get,
        setsolve = setsolve,
        getsolve = getsolve)
}

## The function cacheSolve calculates the inverse of the matrix produced by the function 
## makeCacheMatrix. If the inverse has already been calculated and the matrix is the 
## same, then the function cacheSolve returns the cached version of the inverse. 
## The function assumes the matrix is invertible.     

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
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

