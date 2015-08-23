## First function stores a matrix 'x' and outputs a list containing
## the functions 'get', 'setinv', and 'getinv'.

## makeCacheMatrix:
## get() - function w/o argument that returns matrix 'x' when called
## setinv(*) - function w/ argument that assigns 'solvedMatrix' to variable 'i'
## getinv() - function w/o argument that returns the inverse 'i'
##  Ex: if 'makeCacheMatrix' is assigned to variable 'a', above functions
##  can be called by:
##      a$get()
##      a$setinv(variable)
##      a$getinv()

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    get <- function() x
    setinv <- function(solvedMatrix) i <<- solvedMatrix
    getinv <- function() i
    ##Output list:
    list (get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve:
## Checks if the inverse matrix has been calculated already, and if not,
## then calculates value, assigns to variable 'i', and then caches matrix
## by calling $setinv(i) from makeCacheMatrix function and prints 'i'.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinv()         ##calls getinv() from 'x', where 'x' is a 
                            ##variable with value makeCacheMatrix(matrix)
    if (!is.null(i)) {
        message("getting cached data")
        return(i)
                        ##if getinv() returns a value, then 'i' is returned
                        ##in the if statement
    }
    ## else...
    data <- x$get()     ##assigns matrix to 'data' variable
    i <- solve(data, ...)   ##calculate inverse matrix and assign to 'i'
    x$setinv(i)         ##cache inverse by calling setinv() function
    i                   ##output inverse matrix
}