## Together, these functions cache the inverse of any invertable matrix
## Once it is cached, the inverse can be retrurned for the same matrix, or a new matrix can be
## re-supplied to be inverted and cached

## makeCacheMatrix takes a matrix (or creates a 0 lengthe on if none supplied) and defines the
## functions that can be used to:
## - set the Matrix value in a variable within the function environment
## - get the Matrix from a variable within the function environment
## - set the inverted matrix in a variable within the function environemnt
## - get the inverted matrix from a variable within the function environment

makeCacheMatrix <- function(x = matrix()) {
        iMat <- NULL
        setMat <- function(y) {
                x <<- y
                iMat <<- NULL
        }
        getMat <- function() x
        setinverted <- function(im) iMat <<- im
        getinverted <- function() iMat
        list(setMat = setMat, getMat = getMat,
             setinverted = setinverted,
             getinverted = getinverted)
        
}


## cacheSolve takes

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
        iMat <- x$getinverted()
        if(!is.null(iMat)) {
                message("getting cached inverted matrix")
                return(iMat)
        }
        my_matrix <- x$getMat()
        iMat <- solve(my_matrix)
        x$setinverted(iMat)
        iMat
}