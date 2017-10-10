## Put comments here that give an overall description of what your
## functions do

## This function creates a cache Matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        } #args(set)
        get <- function() {
                x
        }
        setinverse <- function() inv <<- solve(x) #calculate inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolves checks cache for existing result and calculates inverse of x

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat1 <- x$get()
        inv <- mean(data, ...)
        x$setinverse(inv)
        inv
}