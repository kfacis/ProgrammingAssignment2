## The goal of this assignment is to write two functions, one
##which makes a matrix (makeCacheMatrix) and another
##which solves for the inverse of the matrix

## This is the first function which creates the matrix
##object that can be cached for inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        } #args(set)
        get <- function() x {
                x
        }
        setinverse <- function(inverse) inv <<- solve(x) #calculate inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolves is the function which computes the inverse
##of the matrix returned by makeCacheMatrix. If the inverse
##has already been calculated then the cachSolve retrieves
##the inverse from the cache, rather than regenerating
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