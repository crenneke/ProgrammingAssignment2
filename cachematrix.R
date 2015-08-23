## Submission by Christian Renneke for programming assignment 2
## regarding lexical scopring.
## Functions makeCacheMatrix and cacheSolve compute the inverse
## of a matrix and cache it in order to retrieve it instead of
## computing it again.

## makeCacheMatrix stores original and inverse matrix if already 
## calculated in a list object which is returned

makeCacheMatrix <- function(x = matrix()) {
        ## Define inverse matrix result object
        xi <- NULL
        ## Insert original matrix into result list, 
        ## set inverse matrix value to NULL in env above current
        set <- function(y) {
            x <<- y
            xi <<- NULL
        }
        ## Return original matrix
        get <- function() x
        ## Set inverse matrix provided in env above current
        setinverse <- function(inv) xi <<- inv
        ## Get inverse matrix
        getinverse <- function() xi
        ## Return list of function results
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        xi <- x$getinverse()
        if(!is.null(xi)) {
          message("getting cached inverse matrix")
          return(xi)
        }
        data <- x$get()
        xi <- solve(data, ...)
        x$setinverse(xi)
        xi
}