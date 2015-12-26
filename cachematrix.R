
        
## MakeCacheMatrix:This function creates a special "matrix" object that can cache its inverse.


##  cacheMatrix$set(M)      # Change the matrix being cached.
##  M <- cacheMatrix$get()  # Returns the matrix being cached.
##
##  cacheMatrix$setInverse(solve(data, ...)) # Private function containing cached inverse of x
##  cacheMatrix$getInverse()                 # Private function used to get the cached inverse of x

## Usage:
##  M <- matrix(c(1, 2, 3, 4), nrow=2, ncol=2)
##  cacheMatrix <- makeCacheMatrix(M)
##  cacheSolve(cacheMatrix)

## Create a cacheMatrix object for an invertale matrix.

 MakeCacheMatrix <- function(x = matrix()) {
        Cached_Inverse <- NULL
        set <- function(y) {
                x <<- y
                Cached_Inverse <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) Cached_Inverse <<- inverse
        getInverse <- function() Cached_Inverse
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}
      
 #cacheSolve: This function computes the inverse of the special matrix 
 
 
 cacheSolve <- function(x, ...) {
         ## the inverse of 'x' matrix
         inverse_Func <- x$getInverse()
         if(!is.null(inverse_Func)) {
                 message("getting cached data")
                 return(inverse_Func)
         }
         data <- x$get()
         inverse_Func <- solve(data, ...)
         x$setInverse(inverse_Func)
         inverse_Func
 }
