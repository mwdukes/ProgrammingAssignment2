## cachematrix.R
## 
## Pair of functions to provide caching of the solve() function for calculating
## the inverse of a matrix
##
## Two functions are provided:
##      makeCacheMatrix - used to initialize the environment. Returns a list of functions
##      solveCacheMatrix - gets the inverse of the matrix either by returning the cached 
##                         value or by calculating the value and storing it in the cache
## 

## makeCacheMatrix
##      input: matrix x
##      output: list of functions 
##
##      Given a matrix, return a list of functions to perform set, get, set inverse 
##       and get inverse of the matrix.
##
##      Function utilizes the lexical scoping rules to set variables outside of the 
##      defining environment.

makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
    

}


## cacheSolve
##      input: list of functions - output from the makeCacheMatrix function
##      output: the inverse of the matrix set using the makeCacheMatrix
##              a message will be displayed when the value is retrived from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
        m <- x$getinverse()
        if(!is.null(m)) {
            message("getting cached data")
            return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
    }
    
    
    
