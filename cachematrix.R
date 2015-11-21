## Module contains two functions: makeCacheMatrix and cacheSolve
## 
## Usage:
##      cm <- makeCacheMatrix(squareMatrix)
##      cacheSolve(cm)

## This function accepts a square matrix and returns an object that contains 4 functions.
## get() will return the current matrix.
## set(squareMatrix) will change the current matrix to squareMatrix and invalidate
##      the inverse matrix cache.
## setinverse(iSquareMatrix) will set the inverse cache to iSquareMatrix.
## getinverse() will return the value of the inverse matrix cache.
## Note: No checking is done to verify the arguments passed in.

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


## This function expects an object returned by makeCacheMatrix().
## It uses the functions of makeCacheMatrix() to determine if the inverse of matrix
##      has already been solved. If yes, the cached version of the inverse of the 
##      matrix is returned. Otherwise the inverse matrix is calculated and then 
##      stored in the makeCacheMatrix() cache.
## Note: No checking is done to verify the arguments passed in.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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