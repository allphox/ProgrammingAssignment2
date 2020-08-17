## Put comments here that give an overall description of what your
## functions do

## this function get and set a matrix and then set and get an 
## inverse of that function

makeCacheMatrix <- function(x = matrix()) {
        invM <- NULL
        set <- function(y) {
                x <<- y
                invM <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) invM <<- inverse
        getInverse <- function() invM
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function calculates the inverse matrix created 
## with the function makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invM <- x$getInverse()
        if(!is.null(invM)) {
                message("getting cached data")
                return(invM)
        }
        mat <- x$get()
        invM <- solve(mat, ...)
        x$setInverse(invM)
        invM
}
