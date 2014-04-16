################################################################################
## Framework for inverse matrix calculation and storage                       ##
## Due to the high cost of calculating the inverse of a matrix, this functions##
## allow to reduce said cost by calculating the inverse only once and storing ##
## it in a cache for future reference                                         ##
################################################################################

## Define an object that stores a matrix and its inverse, as well as the
## functions used to access and set both matrixes
makeCacheMatrix <- function(x = matrix()) {
    # Store matrix
    x <- x
    # Initialise variable where the inverse matrix will be stored
    inv <- NULL
    
    # Define functions to set and get the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    
    # Define functions to set and get the inverse matrix
    setinv <- function(inv) inv <<- inv
    getinv <- function() inv

    # Return all function definitions together
    retval <- list(set = set, get = get,
                   setinv = setinv,
                   getinv = getinv)
}


## Calculate the inverse of a given matrix and cache it for future reference
## Requires makeCacheMatrix as argument
cacheSolve <- function(x, ...) {
    # Check if an inverse already exists, and return it if it does
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }

    # If the inverse matrix does not exist, then calculate it, and set it
    # in the initial vector passed as argument of the function
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)

    # Return the inverse of the matrix
    inv
}
