## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        ##initialise the  location for the inverted matrix result
        m <- NULL
        
        ## store the unsolved matrix in the cache
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        ## retrieve the unsolved matrix from the cache
        get <- function() x
        
        ## store the inverse of the matrix in the cache
        setinverse <- function(inverse) m <<- inverse
        
        ## pull the inverse of the matrix from the cache
        getinverse <- function() m
        
        ## coerce the names of the available actions into a list
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then cacheSolve should retrieve the inverse from the
## cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        
        ## if there is nothing in the cache, compute it.
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        
        ## empty cache, so do the work
        data <- x$get()
        m <- solve(data, ...)
        
        ##store the result for next time
        x$setinverse(m)
        
        m
}
