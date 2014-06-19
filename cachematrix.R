## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

    # invM will store the cached inverse matrix
    invM <- NULL


    # Setter for the matrix
    set <- function(y) {
        x <<- y
        invM <<- NULL
    }
    # Getter for the matrix
    get <- function() {
	  x
    }


    # Setter for the inverse
    setinvM <- function(inverse) { 
	  invM <<- inverse
    }
    # Getter for the inverse
    getinvM <- function() {
	  invM
    }


    # Return the matrix with our newly defined functions
    list(set = set, get = get, setinvM = setinvM, getinvM = getinvM)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    invM <- x$getinvM()


    # If the inverse is already calculated, return it
    if (!is.null(invM)) {
        message("getting cached data")
        return(invM)
    }


    # The inverse is not yet calculated, so we calculate it
    data <- x$get()
    invM <- solve(data, ...)


    # Cache the inverse
    x$setinvM(invM)


    # Return it
    invM
}
