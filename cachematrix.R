
## The functions below make use of the scoping rules of R to cache the long taking computations and store it in the parent environment of the function.
## The value once computed for a matrix can be fetched multiple times from the cache

## This function to give a list of the functions to be used to set and get the values and means of the matrix

makeCacheMatrix <- function(x = matrix()) {
                                minv <- NULL
        set <- function(y) {
                x <<- y
                minv <<- NULL
        }
        get <- function() x
        setinv <- function(inv) minv <<- inv
        getinv <- function() minv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## A function to reuse the value in cache if found else compute the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
                                minv <- x$getinv()
        if(!is.null(minv)) {
                message("getting cached data")
                return(minv)
        }
        data <- x$get()
        minv <- solve(data, ...)
        x$setinv(minv)
        minv
}

