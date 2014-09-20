# PART 1

### The next function generates a special "matrix" object which is able to
### cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        z <- NULL
        set <- function(y) {
                x <<- y
                z <<- NULL
        }
        get <- function() x
        setInv <- function(cacheSolve) z <<- cacheSolve
        getInv <- function() z
        list(set = set, get = get,
             setInv = setinv,
             getInv = getinv)
}

# PART 2

## Up next, the following function states the inverse of another
## special "matrix" retrieved  by the latter makeCacheMatrix.
## Whenever  the inverse has been previously computed, and morever
## the matrix has not been modified, then the cachesolve should 
## yield the inverse from the cache itself.

cacheSolve <- function(x, ...) {         
        z <- x$getinv()
        if(!is.null(z)) {
                message("getting cached data")
                return(z)
        }
        data <- x$get()
        z <- solve(data) %*% data
        x$setinv(z)
        z
}
