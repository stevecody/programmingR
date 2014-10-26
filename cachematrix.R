## This pair of functions allow the results of a computing intensive process
## (computing the inverse of a Matrix, in this case) to be stored in a 'cache'
## so that the value can be subsequently retreived from the cache rather than
## being recomputed.

## use 'makeCacheMatrix' to create the initial structure
## use 'cacheSolve' to check the cache and return a new or existing (cached) result

## the Matrix object can be inspected  using 'amatrix$get()' and
## 'amatrix$getinverse()' (where 'amatrix' is the name of the object created by
## 'makeCacheMatrix')



## This function creates a special "matrix" object that can cache its inverse.
## It only needs to be run to initially create the structure

makeCacheMatrix <- function(x = matrix()) {
        ##initialise the  location for the inverted matrix result
        m <- NULL
        
        ## store the matrix in the cache and initailise the inverse cache.
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        ## retrieve the matrix from the cache
        get <- function() x
        
        ## store the inverse of the matrix in the cache
        setinverse <- function(inverse) m <<- solve(x)
        
        ## pull the inverse of the matrix from the cache
        getinverse <- function() m
        
        ## coerce the names of the available actions into a list
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}


## This function computes the inverse of the special "matrix" object returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then cacheSolve should retrieve the inverse from the
## cache.

cacheSolve <- function(x, ...) {
        ## Get the inverse of the matrix out of the cache
        m <- x$getinverse()
        
        ## there is something already in the cache, return that.
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
