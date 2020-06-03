## makeCacheMatrix function returns an object with 4 inner functions
## and cacheSolve function solves for an inverse of the given
## makeCacheMatrix object using its inner functions (if there is no cached inverse already)
## and caching the data 

## Creates a "matrix" object that has 4 functions: A getter and a setter
## for a matrix and also a getter and a setter for cached inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    
    setMatrix <- function(y)
    {
        x <<- y
        inverse <<- NULL
    }
    
    getMatrix <- function() x
    
    setInverse <- function(inv) inverse <<- inv
    
    getInverse <- function() inverse
    
    list(setMatrix = setMatrix, getMatrix = getMatrix,
         setInverse = setInverse, getInverse = getInverse)
}


## Solves for given "matrix" object's inverse. 
## First checks if it is not already cached, if it is, just returns it.
## If there is no inverse (NULL value), then solves and caches the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    
    if(!is.null(inv))
    {
        message("getting cached matrix inverse")
        return(inv)
    }
    
    data <- x$getMatrix()
    inv <- solve(data)
    x$setInverse(inv)
    inv
}
