## The following two functions create a special object that caches a matrix 
## and then calculate the inverse of the inverse of the special "matrix". 



## The makeCacheMatrix function creates a special matrix ventor that contains a function to
##  (1) set the value of the matrix
##  (2) get the value of the matrix
##  (3) cache the value of inverse of the matrix
##  (3) return the value of the inverse



makeCacheMatrix <- function(x = matrix()) {
        invrs <- NULL
        set <- function(y) {
                x <<- y
                invrs <<- NULL
        }
        get <- function() return(x)
        setinv <- function(inv) invrs <<- inv
        getinv <- function() return(invrs)
        inv_list <- list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
        return(inv_list)

}


## The cacheSolve function is to compute the inverse of the special "matrix" 
## returned by makeCacheMatrix function above.If the inverse has already been 
## calculated (and the matrix has not changed), then the cacheSolve   
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invrs <- x$getinv()
        if(!is.null(invrs)) {
                message("getting cached data")
                return(invrs)
        }
        data <- x$get()
        invrs <- solve(data, ...)
        x$setinv(invrs)
        return(invrs)
}


