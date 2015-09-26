##############################################################################
# In combination these functions return the inverse of a 2 x 2 matrix
# either by calculation de novo or from a cache they create to store the
# results of these calculations. 
##############################################################################

# This function takes as its argument a 2 x 2 matrix, verifies that it is a 
# matrix with dimensions 2 x 2 and creates a list of four functions which
# get the matrix, set the matrix, get the inverse and set the inverse.

makeCacheMatrix <- function(x = matrix()) {
        if (is.matrix(x) == FALSE | nrow(x)!=2 | ncol(x)!=2) {
                print("This is not a 2 x 2 matrix")
        } else {
                inv <- NULL
                set <- function(y) {
                        x <<- y
                        inv <<- NULL
                }
                get <- function() x
                setinverse <- function(solve) inv <<- solve
                getinverse <- function() inv
                list(set = set, get = get,
                     setinverse = setinverse,
                     getinverse = getinverse)
        }
}


# This function returns the inverse of the matrix output of the first function,
# either from the cache if it is already there or by calculation, in which case
# the result is also cached for later retrieval.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
