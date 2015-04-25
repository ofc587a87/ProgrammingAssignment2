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
    result <- solve(x$get(), ...)
    x$setInverse(result)
    result;
}

#Internal function to test development
testSolve <- function() {
    
    set.seed(Sys.time())
    
    ncols1<-floor(runif(1, 4, 300));
    ncols2<-floor(runif(1, 4, 300));
    
    # creates sample matrices
    message(paste("Generating matrix 1 of" , ncols1 , "cols/rows"))
    M1 <- replicate(ncols1, floor(runif(ncols1, 0, 15)))
    message(paste("Generating matrix 2 of" , ncols2 , "cols/rows"))
    M2 <- replicate(ncols2, floor(runif(ncols2, 0, 100)))
    
    #create cache objects
    cachedM1 <- makeCacheMatrix(M1)
    cachedM2 <- makeCacheMatrix(M2)
    
    #verify data
    if(!identical(M1, cachedM1$get())) { stop("M1 is not identical"); }
    if(!identical(M2, cachedM2$get())) { stop("M2 is not identical"); }
    if(!is.null(cachedM1$getInverse())) { stop("M1 inverse is not null at start"); }
    if(!is.null(cachedM2$getInverse())) { stop("M2 inverse is not null at start"); }
    if(identical(cachedM1$get(), cachedM2$get())) { stop("cached M1 and M2 should not be identical")}
    
    message("Generated matrix OK.")
    
    #solve inverses
    message("Generating inverse 1...")
    cacheSolve(cachedM1);
    message("Generating inverse 2...")
    cacheSolve(cachedM2);
    message("checking...")
    
    #verify cached results
    if(!identical(M1, cachedM1$get())) { stop("M1 is not identical after solve"); }
    if(!identical(M2, cachedM2$get())) { stop("M2 is not identical after solve"); }
    if(identical(cachedM1$get(), cachedM2$get())) { stop("cached M1 and M2 should not be identical")}
    if(is.null(cachedM1$getInverse())) { stop("M1 inverse is null after solve"); }
    if(is.null(cachedM2$getInverse())) { stop("M2 inverse is null after solve"); }
    if(identical(cachedM1$getInverse(), cachedM2$getInverse())) { error("cached inversed M1 and M2 should not be identical")}
    
    #verify results
    if(!identical(solve(M1), cachedM1$getInverse())) { stop("M1 inverse is not right"); }
    if(!identical(solve(M2), cachedM2$getInverse())) { stop("M2 inverse is not right"); }
    if(!identical(M1, round(solve(cachedM1$getInverse())))) { stop("M1 inverse is not right (double inverse check)"); }
    if(!identical(M2, round(solve(cachedM2$getInverse())))) { stop("M2 inverse is not right (double inverse check)"); }

    # All OK    
    message("TEST OK!!!!")
}
