## File:     cachematrix.R
## Author:   Aaron
## Date:     August 18th, 2015
## Modified: August 18th, 2015
##
## Purpose: The purpose of these functions is to be able to cache
##    potentially time-consuming computations. This allows for a computed
##    value to be easily retrieved in the event that the contents of the input
##    are not changing - it can be looked up in cache rather than recomputed.
##    Specifically, in this instance the inverse of matrices are calculated
##    and stored in a cached matrix object.
##

## 'makeCacheMatrix' creates a special "matrix" object that can cache
##    its inverse.
##
## Input(s):
##    'x' - The matrix in which the inverse will be calculated and stored
##        - Default value of matrix()
##

makeCacheMatrix <- function(x = matrix()) {

    ## Initialize to NULL to indicate inverse not calculated
    matrixInverse <- NULL

    ## Set the value of the matrix    
    set <- function(y) {
        x <<- y
        matrixInverse <<- NULL
    }
    
    ## Get the value of the matrix
    get <- function() x
    
    ## Set the inverse value of the matrix
    setInverse <- function(solve) matrixInverse <<- solve
    
    ## Get the inverse value of the matrix
    getInverse <- function() matrixInverse
    
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## 'cacheSolve' computed the inverse of the special "matrix" returned by
##    makeCacheMatrix.  If the inverse has already been calculated
##    (and the matrix has not changed), then cacheSolve should retrieve the
##    inverse from cache.

cacheSolve <- function(x, ...) {

    ## Retrieve matrix inverse value of special matrix list
    matrixInverse <- x$getInverse()
    
    ## Check for NULL value, if not NULL:
    ## Then inverse has been previously calculated -> return value
    if(!is.null(matrixInverse)) {
        message("getting cached data")
        return(matrixInverse)
    }
    
    ## If NULL was present:
    ## Calculate the inverse, then store value in special matrix list and return
    data <- x$get()
    matrixInverse <- solve(data, ...)
    x$setInverse(matrixInverse)
    matrixInverse
}
