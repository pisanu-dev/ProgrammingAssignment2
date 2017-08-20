## R programming assignment week 2 
## Pisanu Chaloemrattanaporn

## functions calculate inverse matrix and store result in cache

## Sample usage:
## cache <- makeCacheMatrix(rnorm(25), nrow = 5, ncol = 5)
## cacheSolve(cache) 



## makeCacheMatrix() will create a matrix for calculation
## The input matrix must be a square and nonsingular matrix

makeCacheMatrix <- function(x = matrix()) {
    
    # function for verify input matrix
    verifyMatrix <- function(m) {
        if (ncol(m) != nrow(m)) {
            message("The input matrix is not square!")  
            return(FALSE)
        }
        
        if (det(x) == 0) {
            message("The input matrix is singular!")  
            return(FALSE)
        }
        
        return(TRUE)
    }
    
    # verify input matrix
    if (verifyMatrix(x) == FALSE) 
        return
    
    
    # mInverse is variable to store cache of inverse matrix 
    mInverse <- NULL 
    
    # Use set() to set a new matrix and initialize mInverse 
    set <- function(y) {
        if (verifyMatrix(y)) {
            x <<- y
            mInverse <<- NULL
        }
    }
    
    # Use get() to returns current matrix
    get <- function() x
    
    # use setInverse() to store inverse matrix
    setInverse <- function(inverse) mInverse <<- inverse
    
    # use getInverse() to get current value of inverse matrix
    getInverse <- function() mInverse
    
    # returns list of function set(), get(), getInverse() and setInverse()
    list(set = set, 
         get = get,
         setInverse = setInverse,
         getInverse = getInverse) 
}


## cacheSolve() will caculate inverse matrix of x that return from makeCacheMatrix()

cacheSolve <- function(x, ...) {
       
    # get inverse matrix from cache
    inverse <- x$getInverse()
    
    # if cache has a value return it
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    
    # in case of no cache value, calculate inverse from original matrix
    data <- x$get()
    
    # calculate inverse matrix
    inverse <- solve(data, ...)
    
    # store result in cache
    x$setInverse(inverse)
    
    # return result matrix
    return(inverse)
}
