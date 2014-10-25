## cachematrix.R
##
## Functions to create a matrix object with potentially precomputed cached inverse matrix
## and to calculate & cache the inverse matrix.   
##
## Example Usage:
##   m <- matrix(1:4, 2,2)       -- A simple invertable 2 x 2 matrix
##   mcm <- makeCacheMatrix(m)   -- Make the makeCacheMatrix
##   mcm$getInverse()            -- Returns NULL since inverse has not yet been computed
##   cacheSolve(mcm)             -- Computes the inverse and caches it
##   mcm$getInverse()            -- Now returns the cached inverse matrix
##
## 2014-10-25 Michael Tague, tague@win.net.   R Programming, Coursera, Assignment #2

## makeCacheMatrix(x = matrix()) - Accepts matrix, returns a makeCacheMatrix
##   $get()                - gets matrix
##   $set(matrix())        - sets matrix while clearing inverse cache
##   $getInverse()         - gets cached inverse matrix (might be NULL)
##   $setInverse(matrix()) - sets cached inverse matrix
makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        get <- function() x
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        getInverse <- function() inverse
        setInverse <- function(inv = matrix()) {
                inverse <<- inv
        }
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## cacheSolve(x, ...) - Accepts a makeCacheMatrix and sets the matrix inverse if not already set.
cacheSolve <- function(x, ...) {
        if(is.null(x$getInverse())) {
                x$setInverse(solve(x$get()))
        } else {
                message("getting cached inverse matrix")
        }
        x$getInverse()
}
        