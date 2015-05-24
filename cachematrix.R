## The functions below create a specail matrix object capable of caching it's inverse
## and a gives an example of it's use.

## create a special matrix object that can cache it's inverse
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
}


## Calculates the inverse of the matrix if it is not cached, and caches the result
cacheSolve <- function(x, ...) {
    i <- x$getinverse();
    if (!is.null(i)) {
        message("using cached value");
        return(i);
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i);
    i
}
