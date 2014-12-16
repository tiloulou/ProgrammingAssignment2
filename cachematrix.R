## Put comments here that give an overall description of what your
## functions do

# Two functions are written here:
# 1. makeCacheMatrix, that creates a special matrix with its inverse once computed once
# 2. cacheSolve, that actually computed the inverse of a matrix, 
#    with caching it into the sepcial matrix structured passed to it
#
# This combination allow for caching computed value of inverse matrices,
# though avoiding repeating the computing.

## Write a short comment describing this function

# The makeCacheMatrix function behaves like makeVector in the example.
# makeCacheMatrix is actually  a list containing a function to :
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse
# 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    inverse_x <- NULL
    set <- function(y) {
        x <<- y
        inverse_x <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inverse_x <<- inverse
    getinverse <- function() inverse_x
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function

# The makeCacheMatrix function behaves like cachemean in the example.
# The following function calculates the inverse of the special "matrix" created with the above function. 
# However, it first checks to see if the inverse has already been calculated. 
# If so, it gets the inverse from the cache and skips the computation. 
# Otherwise, it calculates the inverse of the data and sets the value of the inverse_x in the cache via the setmean function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    inverse_x <- x$getinverse()
    if (!is.null(inverse_x)) {
        message("getting cached data")
        return(inverse_x)
    }
    data <- x$get()
    inverse_x <- solve(x$get())
    x$setinverse(inverse_x)
    inverse_x
}
