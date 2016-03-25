## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix.R creates a matrix object that caches its inverse.
## for this assignment, assume matrix is square always invertible

makeCacheMatrix <- function(x = matrix()) {

 		#if(ncol(x) != nrow(x)) 
        #        stop("error, no inverse for non-square matrix")
                
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


## cacheSolve.R computes inverse of matrix object created with makeCacheMatrix.R
## if inverse has already been calculated (and matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache

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

