## Below are two functions: makeCacheMatrix and cacheSolve
## They are used to create a special "matrix" and cache its inverse.

## Function Example:
## a <- makeCacheMatrix(matrix(1:4,2,2))                         # create a matrix
## a$get()                                                       # get the matrix
## a$set(matrix(5:8,2,2))                                        # create a new matrix
## a$get()                                                       # get the new matrix
## cacheSolve(a)                                                 # compute the inverse
## cacheSolve(a)                                                 # read the cached inverse

## Create a special "matrix" object that can cache its inverse
## It can be used to:
## set the value of the matrix
## get the value of the matrix
## set the value of the inversed matrix
## get the value of the inversed matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() {
                x
        }
        
        setMatrix <- function(matrix) {
                m <<- matrix
        }
        getMatrix <- function() {
                m
        }
        
        list(set = set, get = get,
             setMatrix = setMatrix,
             getMatrix = getMatrix)
}

## Computes the inverse of the special "matrix" from makeCacheMatrix above
## If the inverse has already been calculated (and the matrix has not changed),
## then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getMatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setMatrix(m)
        m
}
