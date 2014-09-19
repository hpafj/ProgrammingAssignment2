### PROGRAMMING ASSIGNMENT 2, COUSERA COURSE R PROGRAMMING ###

## The folowing to functions follows very closely the design pattern outlined
## in the example "Caching the Mean of a Vector scheme" in README.md.
##
## The two functions, when used together, allow for caching the computed
## inverse matrix. 


## makeCacheMatrix: This function creates a special "matrix" object that can
## cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL
    set <- function(newMatrix) {
        x <<- newMatrix
        inv <<- NULL
    }
    get <- function() x
    setInv <- function(solution) inv <<- solution
    getInv <- function() inv
    list(set = set, get = get, setInv = setInv, getInv = getInv)

}


## cacheSolve: This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been
## calculated (and the matrix has not changed), then cacheSolve will
## retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {

    ## Return a matrix that is the inverse of 'x'
    chachedVal <- x$getInv()
    if(!is.null(chachedVal)) {
        message("using cached value")
        return(chachedVal)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInv(inv)
    inv
}
