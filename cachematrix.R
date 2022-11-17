## Caching square matrices and its inverse using lexical scoping.
## This is a part of the Johns Hopkins University course `R Programming`
## hosted on Coursera
## Author: Matthew Hogan (matthewghogan _at_ proton -dot- me)

#' Make a named list of a square-matrix `x` that caches the matrix inverse.
#' The elements of the list are functions
#'  * `set` : Set the matrix value
#'  * `get` : Get the matrix value
#'  * `setinverse` : Set the matrix inverse (not initially calculated)
#'  * `getinverse` : Get the matrix inverse
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(
        set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse
    )
}


#' Get the matrix inverse using the named-list `x` from the `makeCacheMatrix`
#' function. If the inverse is not already calculated, then the inverse is
#' calculated and cached in the named-list
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    return(m)
}
