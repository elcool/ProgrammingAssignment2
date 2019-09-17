## Put comments here that give an overall description of what your
## functions do

## This function will create an object with a special matrix with the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    message("functions created. returning list")
    list(set = set, 
         get = get, 
         setsolve = setsolve, 
         getsolve = getsolve)
}


## cacheSolve will control the cache if the solve() has already been called for x
cacheSolve <- function(x, ...) {
    m <- x$getsolve()
    if (!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)  # the actual place to solve
    x$setsolve(m)
    m
}
