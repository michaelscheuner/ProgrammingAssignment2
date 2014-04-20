## Calculates and caches the inverse of matrix.
## Example:
## k = makeCacheMatrix(matrix(c(4,3,3,2), 2, 2))
## cacheSolve(k)
## Second call of cacheSolve(k) accesses the cached result.

## Creates the special matrix with cache-functionality.
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    # set a new matrix, clear the inverse-cache.
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    # return the matrix
    get <- function() x
    # set and get the inverse of the matrix.
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    # return a list of the 4 functions.
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Calculates the inverse of a special matrix and stores
## it in the objects cache.
cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    # check, if the inverse was already calculated.
    if(!is.null(m)) {
        # if yes, return the cached version
        message("getting cached data")
        return(m)
    }
    # otherwise calculate the inverse with 'solve',
    # set the result in the cache and return it.
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
