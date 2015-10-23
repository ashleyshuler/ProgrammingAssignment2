## The pair of functions makeCacheMatrix and cacheSolve work together to return the
## inverse of a matrix (stored in a cache) rather than recomputing it again if the 
## input matrix has not changed. If the input matrix has changed, the inverse is 
## calculated normally.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), then
## cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
            message("getting cached inverse")
            return(m)
        }
        data <- x$get()
          m <- solve(data, ...)
          x$setinverse(m)
          m
}
