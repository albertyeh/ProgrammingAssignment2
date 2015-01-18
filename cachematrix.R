## Put comments here that give an overall description of what your
## functions do

## Function: makeCacheMatrix
## Description: Create a cache matrix object 
makeCacheMatrix <- function(x = matrix()) {
    ## Initial inverse matrix 
    m <- NULL
    
    ## set/update matrix 
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    ## get current matrix
    get <- function() x
    
    ## set/update inverse matrix
    setinverse <- function(inverse) m <<- inverse
    
    ## get current inverse matrix
    getinverse <- function() m
    
    ## return CacheMatrix object(a list)
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Function: cacheSolve
## Description: Return inverse value (first time is non-cahced)
cacheSolve <- function(x, ...) {
    ## get inverse matrix from cache
    m <- x$getinverse()  
    
    ## Return it if inverse matrix is cached 
    if(!is.null(m)) {
        message("getting cached data")
        ## Get inverse matrix from cache and return it.  
        return(m)   
    }
    
    ## Calculate inverse matrix
    data <- x$get()
    m <- solve(data, ...)

    ##Save inverse matrix into cache
    x$setinverse(m)
    
    ## return inverse matrix(non-cached)
    m 
}
