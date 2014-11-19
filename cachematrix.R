## This pair of functions will cache the inverse of a matrix rather
## than compute it repeatedly as matrix inversion is usually a
## costly computation.

## Function makeCacheMatrix creates a special "matrix" along with
## a list containing a function to set and get the value of the
## matrix and to set and get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Function cacheSolve calculates the inverse of the special
## "matrix" created with the function makeCacheMatrix.
## However, it first checks to see if the inverse has already
## been calculated. If so, it gets the inverse from the cache and
## skips the computation. Otherwise, it calculates the inverse of
## the data and sets the value of the inverse in the cache via
## the setinverse function.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
