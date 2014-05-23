## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        ## set the matrix; and clear the cashed inverse
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        ## get the matrix
        get <- function() x
        ## set the matrix inverse
        setinverse <- function(inverse) m <<- inverse
        ## get the matrix inverse
        getinverse <- function() m
        ## return a list
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getinverse()
        ## if inverse is cached, return it
        if(!is.null(m)) {
                message("getting cached inverse")
                return(m)
        }
        
        ## otherwise, sovle the matrix, set the inverse, and then return the inverse
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m
}