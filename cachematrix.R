## Function that returns a list with functions that provide the Inverse of
## a Matrix if it has been previously computed

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(y) inv <<- y
    getinv <- function() inv
    list(set = set,get = get, setinv = setinv, getinv = getinv)
}


## Function that uses the cache to get the Inverse of a Matrix previously 
## calculated, else it computes the function solve on the Matrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)){
        return(inv)
    }
    M <- x$get()
    inv <- solve(M, ...)
    x$setinv(inv)
    inv
}
