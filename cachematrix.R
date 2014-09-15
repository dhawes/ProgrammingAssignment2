## Functions to handle solving the inverse of a matrix.
##
## Matrices can be costly to compute, so these functions allow us to cache a
## matrix to be used later, avoiding the cost of computing it every time.

## Create a cache matrix that is a list which contains functions to get and set
## the matrix and inverse matrix.
##
## Parameters:
##    x - an invertible matrix
makeCacheMatrix <- function(x = matrix())
{
    ix <- NULL

    set <- function(y)
    {
        x <<- y
        ix <<- NULL
    }

    get <- function() x

    setinverse <- function(inverse) ix <<- inverse

    getinverse <- function() ix 

    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Given a cache matrix x returned from makeCacheMatrix(), solve the inverse
## matrix. If the inverse has already been computed, return the cached copy.
##
## Parameters:
##    x - a cache matrix returned from makeCacheMatrix()
cacheSolve <- function(x, ...)
{
    ## Return a matrix that is the inverse of 'x'
    ix <- x$getinverse()
    if(!is.null(ix))
    {
        message("getting cached data")
    }
    else
    {
        ix <- solve(x$get())
        x$setinverse(ix)
    }
    ix
}
