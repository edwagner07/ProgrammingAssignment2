## The first part of the script creates a list of functions that 
## retrieve and manipulate the inverse of a matrix. The second
## part actually calculates this inverse - it then uses functions 
## from the first portion to define the inverse as a variable.


## makeCacheMatrix
## First, a matrix can be inputted. Then, a list of functions is
## created - these can be easily called later to check or update
## the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {

    m <- NULL
    set <- function(y)  {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(inv) m <<- inv
    getinv <- function() m
    list(set = set,get = get, 
         setinv = setinv, getinv = getinv)
}



## cacheSolve
## This function first checks to see if the matrix has been solved.
## It then solves the inverse of the matrix, ONLY IF the matrix has
## not yet been solved - otherwise it returns the cached value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data,...)
    x$setinv(m)
    m
    
    
}



