## This function operates on a matrix.
## This function first sets the value of the matrix,
## then gets the value of the matrix 
## Then calculates and sets the value of the inverse
## then gets the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function works in conjunction with the above function.
## It searches through the cache data to find the already calculated inverse 
## of the matrix fed to it. If the inverse is not found, then it calls on the 
## function above and calculates and returns the inverse. 

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
        ## Return a matrix that is the inverse of 'x'
}
