## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than compute it repeatedly.

## This is a pair of functions that cache the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y){
        x <<- y
        s <<- NULL
    }
    get <- function()x
    setmatrix <- function(solve) s <<- solve
    getmatrix <- function() s
    list(set=set, get=get, setmatrix=setmatrix, getmatrix=getmatrix)
}


## This function creates a special "matrix" object that can cache its inverse.

cacheSolve <- function(x, ...) {
    s <- x$getmatrix()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setmatrix(s)
    s
        ## Return a matrix that is the inverse of 'x'
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix
## above. If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
