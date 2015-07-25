## Creates a type of matrix which can cache the inverse.

## Exemple of use :

## mat <- makeCacheMatrix (matrix (data = c(1, 2, 3, 4), nrow = 2))
## print (cacheSolve (mat))

## The function makeCacheMatrix returns a vector of four functions set, get, setSolve and getSolve
## similar to the exemple with the mean of a vector (here 'solve' stands for 'inverse').
## Exemple of use : mat <- makeCacheMatrix (matrix (data = c(1, 2, 3, 4), nrow = 2))

makeCacheMatrix <- function (x = matrix()) {
    s <- NULL
    set <- function (y) {
        x <<- y
        s <<- NULL
    }
    get <- function () x
    setSolve <- function (solve) s <<- solve
    getSolve <- function () s
    list (set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve)
}

## Function cacheSolve :
## If the inverse of the cacheMatrix already exists, returns it.
## Else : computes the inverse of the cached matrix with the 'solve' function, and stores it in the cache.

cacheSolve <- function (x, ...) {
    ## Return a matrix that is the inverse of 'x'
    s <- x$getSolve ()
    if (!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve (data, ...)
    x$setSolve(s)
    s
}
