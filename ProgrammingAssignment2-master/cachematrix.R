## The functions here cache inverse of a matrix so that it 
## not calculated again if the matrix is the same.

## This function caches the value and creates functions for the same


makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)


}


## This function returns the cached value if the value of inverse remains same

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("Inverse of matrix is:")
                return(inv)
        }
        data <- x$get()
        inv <- solve(x)
        x$setinv(inv)
        inv
}
