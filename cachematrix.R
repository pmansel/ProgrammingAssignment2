##----------------------------------------------------------
## makeCacheMatrix: Creates a 'special' matrix object that
##  comes with a setter/getter methods, along with methods
##  that will set/get the value of the inverse of the matrix
##  into a cache for optimization purposes.
##
## cacheSolve: TODO
##----------------------------------------------------------




##----------------------------------------------------------
## makeCacheMatrix
##----------------------------------------------------------
## Arguments:
##  x: invertible matrix
##
## Return value:
##  list that contains one element for each of the functions
##  used to interact with the matrix.
##----------------------------------------------------------
## Description:
##  The function will create a list of functions that will
##  allow to interact with the matrix:
##      - get: Returns the matrix itself
##      - set: Sets the value of the matrix (will also 
##          reset the cached value for the matrix inversion)
##      - setmatrixinverse: Stores the inverse of the matrix 
##          in the cache for later retrieval
##      - gettmatrixinverse: Retrieves the cached value for 
##          the matrix inverse
##
##----------------------------------------------------------
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL

    ## set function: stores the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }

    ## get function: returns the matrix
    get <- function() {
        return(x)
    }

    ## setmatrixinverse function: stores the matrix inverse in the cache
    setmatrixinverse <- function(i) {
        inv <<- i
    }

    ## getmatrixinverse function: returns the matrix inverse from the cache
    getmatrixinverse <- function() {
        return(inv)
    }

    ## Returns the list with matrix function handlers
    list(set = set, get = get, setmatrixinverse = setmatrixinverse,
           getmatrixinverse = getmatrixinverse)
}



##----------------------------------------------------------
## cacheSolve
##----------------------------------------------------------
## Arguments:
##  x: invertible matrix
##
## Return value:
##  Matrix that is the inverse of 'x'
##----------------------------------------------------------
## Description:
##  The function checks whether the inverse has already been
##  calculated by trying to retrieve it from the cache.
##  If the value is available from the cache, it will be
##  instantly returned, otherwise, the inverse of the matrix
##  will first be calculated, then saved into the cache, and
##  eventually returned.
##----------------------------------------------------------
cacheSolve <- function(x, ...) {
    ## Attempt to retrieve the inverse from the cache
    inv <- x$getmatrixinverse()

    ## Value was found in the cache, just return it
    if (!is.null(inv)) {
        message("getting cached data") 
        return(inv)
    }

    ## If we're here, it means that the cache was empty.
    ## Let's retrieve the matrix first
    data <- x$get()

    ## Now, let's compute the inverse of the matrix
    ## Note: We assume that the matrix is invertible.
    inv <- solve(data)

    ## Cache the value
    x$setmatrixinverse(inv)

    return(inv)
}
