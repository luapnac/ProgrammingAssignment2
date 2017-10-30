## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## The following function creates and caches a matrix (object) that represents 
## the inverse of the input matrix. Assumption: input is a invertible square 
## matrix

makeCacheMatrix <- function(x = matrix()) {
    
    invert <- NULL
    set <- function(y) {
        x <<- y
        invert <<- NULL
    }
    get <- function() x
    setinvert <- function(solve) x <<- solve
    getinvert <- function () invert
    list(set = set, get = get, setinvert = setinvert, getinvert = getinvert)

}

## Write a short comment describing this function

## If the inverse of the matrix has not yet been calculated in makeCacheMatrix,  
## then the following function cacheSolve calculates the inverse of the matrix.  
## If the matrix has already been calculated, then it's retrieved from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    invert <- x$getinvert()
    if(!is.null(invert)) {
        message("retrieving cached data")
        return(invert)
    }
    data <- x$get()
    invert <- solve(data, ...)
    x$setinvert(invert)
    invert
    
}
