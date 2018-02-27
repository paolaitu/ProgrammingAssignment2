## This function creates a special matrix
## function caches Matrix's inverse 


makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}



## This function will execute the solve to get 
## the matrix's inverse, it will validate if it's different to null to 
## retrieve, if null, it will calculate the inverse, set the inverse and return it
## The fact of returning a list() allows access to any other objects defined in the env. 
## of the original function. The subsequent code can access the values of x or m, thought 
## the use of getters and setters. We can also access like: myMatrix_object$getsolve()
cacheSolve <- function(x, ...) {
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}
