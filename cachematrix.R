## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    setmat <- function(mymat) {
        x <<- mymat
        inv <<- NULL
    }
    
    getmat <- function() x
    
    setinv <- function(myinv) inv <<- myinv
    
    getinv <- function() inv
    
    list(setmat = setmat,
         getmat = getmat,
         setinv = setinv,
         getinv = getinv
    )
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    s <- x$getinv()
    if(!is.null(s)) {
        ## message("getting cached data")
        return(s)
    }
    data <- x$getmat()
    s <- solve(data)
    x$setinv(s)
    s
}
