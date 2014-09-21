## makeCacheMatrix creates a list of functions which allow
## for the storage of the inverse of a matrix in cache. X
## and s are defined so that they can be accessed outside of
## the function.
## makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setInv <- function(solve) s <<- solve
        getInv <- function() s
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}

# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.
# The solve function assumes a square matrix is given

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getInv()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setInv(s)
        s
}
