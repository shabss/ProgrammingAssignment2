## Provides functionality to inverse a matrix 'x'
## The inverse is stored and retrived from cache

## Creates a list with functions, which are used 
## to get a cached matrix. 

makeCacheMatrix <- function(x = matrix()) {
    
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    list(set = set, get = get,
         setsolve = setsolve, 
         getsolve = getsolve)
}

## Returns a matrix that is the inverse of 'x'. 
## If matrix 'x' inverse is already in the cache
## then returns it otherwise creates an inverse and 
## stores it in cache before returning.
## The input matrix 'x' must be created using makeCacheMatrix
## function above

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    s <- x$getsolve()
    if (!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s
}


