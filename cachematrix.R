## Solution to Programming Assigment 2: Lexical Scoping
## Author: ofc587a87
## URL: https://github.com/ofc587a87/ProgrammingAssignment2

## this function create a matrix capable of storing a cache of its own inverse

makeCacheMatrix <- function(x = matrix()) {
    
    # inverse cache
    inverse <- NULL
    
    # initialize the object with a new matrix
    # empty the inverse cache as the old one is not valid
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    
    # return current matriox (not inverse)
    get <- function() { x }
    
    # sets and save cache of the inverse matrix
    setInverse <- function(inverseMatrix) {
        inverse <<- inverseMatrix
    }
    
    # get current inverse.
    # return NULL if the inverse has notr been set
    getInverse <- function() { inverse }
    
    # return list of existing functions
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Return a matrix that is the inverse of 'x'
## this function does not cache anything

cacheSolve <- function(x, ...) {
    # get current inverse (if cached)
    result = x$getInverse()
    
    # if it's cached, return data
    if(!is.null(result)) {
        return(result)
    }
    
    # it's not cached, solve ans save it
    result <- solve(x$get())
    x$setInverse(result)
    result;
}
