# makeCacheMatrix
# create a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        
        # reset inverse to NULL
        reset <- function(a) {
                x <<- a
                inv <<- NULL
        }
        
        # return main matrix.
        returnm <- function() {x}
        
        # set inverse of the matrix.
        setinv <- function(inverse) {inv <<- inverse}
        
        # return inverse of the function.
        returninv <- function() {inv}
        
        # return list of functions
        list(reset=reset,
             returnm=returnm,
             setinv=setinv,
             returninv=returninv)
}

# cacheSolve
# computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        # return current version of inverse
        inv <- x$returninv()
        
        # if inverse is already computed, return cache
        if(!is.null(inv)) {
                message('getting cached inverse')
                return(inv)
        }
        
        # compute and cache inverse
        originalm <- x$returnm()
        inv <- solve(originalm)
        x$setinv(inv)
        
        # return inverse
        inv
}
