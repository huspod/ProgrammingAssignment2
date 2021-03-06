##  The pair of functions 'makeCacheMatrix' and 'cacheSolve' cache the inverse of a matrix. Assumes matrix supplied 
## is invertible. 'cacheSolve' calls 'MakeCacheMatrix' to determine if matrix inverse has already been previously 
## computed to save on computation. Otherwise, matrix inverse is computed using the 'solve' function.


## Create a special matrix object that can cache its inverse.  

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
##Set the value of the matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
##Get the value of the matrix        
        get <- function() x
##Set the value of the inverse        
        setinverse <- function(solve) m <<- solve
##Get the value of the inverse        
        getinverse <- function() m
##Return special matrix object for use in cacheSolve
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}




## Computes matrix inverse using 'solve', unless matrix inverse has already been calculated in which case it 
## will be retrieved from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
#
##Query the cache
      m <- x$getinverse()
##If there is a cache, return the inverse already calculated       
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
##Matrix inverse has not been previously calculated, so calculate it now        
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m

}
