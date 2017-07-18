## Put comments here that give an overall description of what your
## functions do
## Write a short comment describing this function

## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly. 
## a pair of functions that are used to create a special object that stores a matrix and caches its inverse

## function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
 inv <- NULL                                    ## initialize inv as NULL
 set <- function(y) {                           ## define  
        x <<- y                                 
        inv <<- NULL                            ## if there is a new matrix, reset inv to NULL
        }
 get <- function() x                            ## define the get function
        
 setInverse <- function(inverse) inv <<- inverse        ## assigns value of inv in parent environment
 getInverse <- function() inv                           ## gets the value of inv where called

 list(set = set,
      get = get,
      setInverse = setInverse,
      getInverse = getInverse)
}


## Write a short comment describing this function

## function computes the inverse of the special matrix created by makeCacheMatrix above
## if the inverse has already been calculated and the matrix has not changed, it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
           message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
