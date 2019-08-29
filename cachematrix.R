## This is a short R script meant to provide the functionality of taking
##  an invertable matrix as input, inverting it, caching the inverted matrix,
##  and returning the inverted matrix as an output. It is meant to save computation time.

## makeCacheMatrix is a function that converts a matrix to an object with methods to
##  set & get both the matrix and its inverse, caching the two in the process.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInv <- function(inverseInput) inv <<- inverseInput
        getInv <- function() inv
        list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## cacheSolve is meant to either get the cached inverse of the matrix or, if the inverse
##  is not already cached, solve() for the inverted matrix, then cache and return it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInv()
        if(!is.null(inv)) {
                message("getting cached inverse data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setInv(inv)
        inv
}
