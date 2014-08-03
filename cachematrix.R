##
## Matrix Inversion Caching
###

## Creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(m = matrix()) {
        i <- NULL
        set <- function(v){
                m <<- v
                i <<- NULL
        }
        get <- function() m
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set= set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Computes the inverse.
## If the inverse is already computed, 
## then the cachesolve should retrieve the inverse from the cach

cacheSolve <- function(m, ...) {
        i <- m$getinverse()
        if (!is.null(i)){
                message("getting cached data")
                return(i)
        }
        data <- m$get()
        i <- solve(data, ...)
        m$setinverse(i)
        i
}

