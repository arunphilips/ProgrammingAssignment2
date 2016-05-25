##These functions are used to cache the inverse of a matrix, for quick access.

## This function, makeCacheMatrix, creates a special list, which is really a list 
## containing a function to set the value of the vector, get the value of the vector
## set the value of the inverse and get the value of inverse.


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get,
             setinverse = setinv,
             getinverse = getinv)
}



##The following function calculates the inverse of the matrix created with
##the previous function. However, it first checks to see if the inverse has already been 
##calculated. If so, it gets the inverse from the cache and skips the computation. Otherwise,
##it calculates the inverse of the data and sets the value of the inver in the cache via
##the setinverse function.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
        ## Return a matrix that is the inverse of 'x'


