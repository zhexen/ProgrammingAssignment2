## This file is for the Programming Assignment 2: Lexical Scoping of the R 
## Programming
##
## Functions in this file include makeCacheMatrix and cacheSolve.
## 
## The function makeCacheMatrix creates a special "matrix" object that can
## cache its inverse.
## 
## The function cacheSolve computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.

## The function makeCacheMatrix creates a special "matrix" object that can
## cache its inverse.
## Usage: m <- makeCacheMatrix(x)
## Input: x should be an invertible matrix
## Output: m will a special "matrix" object which has the same value as the 
## input matrix x, but m can "remember" its inverse.
makeCacheMatrix <- function(x = matrix()) {
        
        # The object uses i to cache the inverse of the input matrix
        # The initialization of i
        i <- NULL
        
        # The definition of 'set' method
        # The caller will use this method to assign matrix to the object
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        # The definition of 'get' method
        # The caller will use this method to get the matrix
        get <- function() x
        
        # The definition of 'setinverse' method
        # The object will use this method to cache the inverse of the matrix
        setinverse <- function(inverse) i <<- inverse
        
        # The definition of 'getinverse' method
        # The object will use this method to return the inverse of the matrix
        getinverse <- function() i
        
        # Construct the function list
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The function cacheSolve returns the inverse of the special matrix
## Usage: v <- cacheSolve(m)
## Input: m should be a special matrix object
## Output: v is the inverse matrix of the input matrix object
cacheSolve <- function(x, ...) {
        
        # The function tries to get the inverse matrix from the cache
        i <- x$getinverse()
        
        # If the cached result is got, then return the result and print 
        # corresponding messages
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        
        # If there is no cached result, the function need to get the matrix, 
        # calculate the inverse, and then cache it.
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        
        # return the inverse matrix as a result
        i
}
