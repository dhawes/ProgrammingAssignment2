## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

cacheSolve <- function(x, ...)
{
    ## Return a matrix that is the inverse of 'x'
    ix <- x$getinverse()
    if(!is.null(ix))
    {
        message("getting cached data")
        return(ix)
    }
    data <- x$get()
    ix <- solve(data)
    x$setinverse(ix)
    ix
}
