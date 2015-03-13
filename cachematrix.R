## This pair of functions calculates the inverse of a matrix.
## Caching is used to optimise calculation time.
##
## Example of typical usage:
##      m <- matrix(c(1,2,3,4), 2, 2)   ## define invertible matrix
##      cm <- makeCacheMatrix(m)        ## make the cacheable matrix
##      m_inv <- cacheSolve(cm)         ## calculate the inverse matrix of m


## Function to create a "cache matrix" for storing both the
## input matrix and the cached inverse matrix (if calculated).
##
## INPUT:       Invertible/non-singular matrix.
## RETURNS:     A list of functions that "cacheSolve" (below) uses for caching
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        
        # return statement
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Function to return the inverse matrix.
##
## If the inverse matrix is already calculated, the cached inverse matrix
## is returned. Otherwise it is calculated with "solve", then cached, 
## then returned.
##
## INPUT:       "cache matrix" created with "makeCacheMatrix" (above)
## RETURNS:     Inverse of the matrix "x$get"
cacheSolve <- function(x) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                return(inv)     ## return cached inverse matrix if already calculated
        }
        ## if inverse matrix is not cached, calculate the inverse with "solve"
        data <- x$get()         ## get input matrix
        inv <- solve(data)      ## calculate inverse matrix
        x$setinv(inv)           ## cache inverse matrix
        
        ## return statement
        inv
}