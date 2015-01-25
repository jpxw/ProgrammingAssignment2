## A pair of functions for working with cached matrices and their inverses
## The inverse of a matrix is cached the first time it has to be calculated
## and the cached value is returned if subsequently needed, without recalculation
##
## Usage:
##
## cm <- makeCacheMatrix(m) - Make a cached matrix cm from a regular matrix m
## cm$get()                 - Get the matrix cached in cm
## cm$set(m1)               - Set the cached matrix to m1
## cm$getinv()              - Get the inverse of the cached matrix
## cm$setinv(i1)            - Internal function for setting value of inverse
##
## WARNING: Matrices used are assumed to be square and invertible!



## makeCacheMatrix - Make a cached matrix from a regular matrix

makeCacheMatrix <- function(x = matrix()) {
    
    # x - a matrix to cache, assumed to be invertible
    
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)

}


## cacheSolve - Return the inverse of a cached matrix

cacheSolve <- function(x, show.msg=FALSE, ...) {
    
    # x - a cached matrix, created by makeCacheMatrix
    # show.msg - control whether messages are printed
    #   (set to TRUE for debugging etc, FALSE for normal use)
    
    inv <- x$getinv()
    if(!is.null(inv)) {
        if (show.msg) {
            message("getting cached data")
        }
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
