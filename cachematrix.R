## If the inverse of a matrix has to be calculated several times, with these functions 
## it s only newly calculated, if there is not yet an inverse in cache. 
## Otherwise the inverse is simply taken from the cache instead of recalculating it.




## The function makeCacheMatrix returns a list of the 4 functions set, get, setinverse, and getinverse.

## The function set sets the matrix x to be y (a matrix input) and sets the object that 
## stores the inverse to NULL, so that a new inverse can be calculated.

## The get function returns the matrix x.
## Function setinverse sets the value of variable inv to be the function 'solve' in the main function
## Function getinverse returns the value of inv 



makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL                      
        set <- function(y) {
                x <<- y         
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inv <<- solve
        getinverse <- function() inv
        list(set = set, get = get,        
             setinverse = setinverse,
             getinverse = getinverse)
}



## Cachesolve takes makeCacheMatrix as input. It then runs the function getinverse, 
## which is an element of the list of functions constituting makeCachenatrix and assigns it to object inv.
## If there's already an object inv in cache, it gives a message and returns inv from cache.
## Otherwise it takes the matrix, calculates the inverse, writes  it to cache and returns it.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)    
        x$setinverse(inv)
        inv
}

