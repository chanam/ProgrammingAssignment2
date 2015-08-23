## The below functions save a matrix and the inverse of the matrix to cache if
## the inverse has already been calculated.  If the inverse of the matrix hasn't
## been calculated, cacheSolve uses solve() to return the inverse and save to
## cache.

## makeCacheMatrix creates a 'special' matrix that consists of 4 functions to 
## set and get the values of a matrix and the matrix's inverse to/from cache

makeCacheMatrix <- function(x = matrix()) {
    B <- NULL
    set <- function(y) {
        A <<- y
        B <<- NULL
    }
    get <- function() A
    setinverse <- function(inverse) B <<- inverse
    getinverse <- function() B
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve returns the inverse of a matrix.  First it checks if the inverse
## is stored in cache and returns this value, otherwise it calculates the
## inverse with solve() and saves this to cache.

cacheSolve <- function(A, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    B <- A$getinverse()
    if(!is.null(B)) {
        message("getting cached data")
        return(B)
    }
    data <- A$get()
    B <- solve(data, ...)
    A$setinverse(B)
    B
}
