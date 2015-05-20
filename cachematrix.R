## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it 
## repeatedly, bellow is a pair of functions that cache the inverse of a matrix
## 
## These functions assume that the matrix supplied is always invertible it means
## it must be square otherwise native function solve will throw an error.
## 
## Usage: 
## > x <- cbind(c(4, 2), c(7, 6))
## > x
##      [,1] [,2]
## [1,]    4    7
## [2,]    2    6
## 
## > m <- makeCacheMatrix(x)
## > cacheSolve(m)
##      [,1] [,2]
## [1,]  0.6 -0.7
## [2,] -0.2  0.4
##
## > cacheSolve(m)
## getting cached data
##      [,1] [,2]
## [1,]  0.6 -0.7
## [2,] -0.2  0.4

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated then
## it retrieve the inverse from the cache rather 

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        ## Return a matrix that is the inverse of 'x'
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    ## Return a matrix that is the inverse of 'x'
    m
}
