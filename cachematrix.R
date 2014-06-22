## The first function accepts a matrix as input and returns it's inverse. If the
## inverse is in the cache that is returned, otherwise the inverse is 
## calculated. This stops repeated calculations.

## Constructor function. Accepts a matrix as input and returns a list 
## containing 

makeCacheMatrix <- function(x = matrix()) {
        # inv stores the inverse of the matrix. Start by setting to NULL
        inv <- NULL
        
        # set the matrix x
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        # get the matrix x        
        get <- function() x
        
        # set the inverse of the matrix x
        setInverse <- function(Inverse) inv <<- Inverse
        
        # get the inverse of the matrix x
        getInverse <- function() inv
        
        # creates a list containing 4 functions (set, get, setInverse and getInverse)
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)     
}


## Accepts the matrix and returns the inverse. 
## Calls makeCacheMatrix and returns the inverse from cache if it exists
## or calculates it

cacheSolve <- function(x, ...) {
        ## Return the inverse of 'x' if it is already cached
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        # if not cached, calc the inverse
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        
        # return inverse
        inv
}