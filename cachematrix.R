## makeCacheMatrix function creates a matrix that contains the functions:
## set, get, set inverse, get inverse

makeCacheMatrix <- function(x = matrix()) 
{
        i <- NULL
        set <- function(y)
    {
        x <<- y
        i <<- NULL
    }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(
        set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}

## cacheSolve calculates the inverse of the matrix checking if it was calculated before, 
## if its calculated brings the result, if not it proceeds.


cacheSolve <- function(x, ...) 
{
        i <- x$getinverse()
        if(!is.null(i)) 
        {
        message("getting cached data")
        return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
