## Put comments here that give an overall description of what your
## functions do
# This function is required to perform the Inverse Matrix Solve once 
# and then cache the solution in the memory.
# If the function is called again it would return the values from the cache.
# 
# For example -
#         Use Matrix 
#         M <- matrix(c(1,2,3,4), nrow=2, ncol=2)
#         It returns the inverse of the matrix


# Create a makeCacheMatrix object for the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(invert) m <<- invert
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
        
}


# Returns the inverse of the CacheMatrix object

cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}
