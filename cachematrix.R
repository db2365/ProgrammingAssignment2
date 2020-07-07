## Put comments here that give an overall description of what your
## functions do

## x: a square invertible matrix
## return: a list containing functions to
##              1. set the matrix
##              2. get the matrix
##              3. set the inverse
##              4. get the inverse
## this list forms first function, is used as input for cacheSolve()

## Write a short comment describing this function
## This first function sets/creates the matrix and the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) { ## Set value of the matrix
                x <<- y
                m <<- NULL
        }
        get <- function() x ##get value of matrix
        setinv <- function(inverse) m <<- inverse ##set value of inverse
        getinv <- function() m ## get the value of the inverse
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
## This function computes the inverse of the matrix created with the previous function

cacheSolve <- function(x, ...) {
        m <- x$getinv() ## returns a matrix that is the inverse of x and assigns it to m
        if(!is.null(m)){ ## if inverse is going to be returned from cache, gives following message
                message("Getting cached data")
                return(m) ## inverse will be returned
        }
        data <- x$get() ## If not in cache, have to compute the inverse of the matrix
        m <- solve(data, ...)
        x$setinv(m) ## set value of inverse in cache
        m
        ## Return a matrix that is the inverse of 'x'
}
