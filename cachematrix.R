# cachematrix.R file contains a sequence of two functions which receives a 
# (square) matrix as input, creates a special matrix object, and computes its 
# inverse

# The makeCacheMatrix function receives a matrix as input and creates a 
# special matrix object. The returned object is basically a list with four 
# functions that allows the user to access and change the object content:
#    1) set:    Rewrites the object's matrix based on a new input matrix 
#               and sets its inverse to null
#    2) get:    Returns the stored matrix
#    3) setinv: Caches the inverse of the stored matrix
#    4) getinv: Returns the inverse of the stored matrix

makeCacheMatrix <- function(x = matrix()) {
        # x, y, and inv are matrices
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}

# The cacheSolve function receives the special matrix object created with 
# makeCacheMatrix function as input and computes, caches, and returns its 
# inverse. If the inverse has already been calculated, then the cacheSolve 
# function retrieves the inverse from the cache. If the matrix has changed, 
# cacheSolve calculates and caches the inverse again (when using the set 
# function from makeCacheMatrix to change the input matrix the inverse is 
# set to NULL).

cacheSolve <- function(x, ...) {
        # x is a special matrix object created using the makeCacheMatrix 
        # function
        # i and data are matrices
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}
