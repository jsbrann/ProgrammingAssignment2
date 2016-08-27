## Matrix inversion is usually a costly computation 
## and there may be some benefit to caching the inverse of a matrix 
## rather than computing it repeatedly. These functions do that.


## Creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
 		  m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
			## does this line return m, set m, or both??? assuming both
        setmatrix <- function(mtx) m <<- mtx
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}

## Computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated, 
## then cacheSolve retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
        m <- x$getmatrix()
        if(!is.null(m)) {
					
                message("getting cached data")
                return(m)
					
        }
        data <- x$get()
        m <- solve(data)
        x$setmatrix(m)
        m
}
