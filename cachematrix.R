## There are two main functions defined here:
## (1) makeCacheMatrix which defines a set of subfunctions
## (2) cacheSolve which retrives special matrix and solves inverse - uses functions defined in (1)
## functions do

## makeCacheMatrix creates a special matrix object (m) that can cache it's inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
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


## cacheSolve computes the inverse of the special matrix. If allready calculated, retrieves inverse from cache.

cacheSolve <- function(x, ...) {
 
      
        m <- x$getinverse()
        ##getinverse and setinverse are defined in makeCacheMatrix function above
       
        ## Check is matrix has been previously cached...
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
                
}
