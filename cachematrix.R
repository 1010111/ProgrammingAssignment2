## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function will make a cache matrix 
## This function also make list of usable function that will be accessible via $
makeCacheMatrix <- function(x = matrix()) {
        ## initialize inverse matrix
        inverseVar <- NULL
        ## create a setter for normalMatrix
        setMatrix <- function(normalMatrix) {
                x <<- normalMatrix
                inverseVar <<- NULL
        }
        ## return matrix set by above function
        getMatrix <- function() {
                x
        }
        ## set result of inverse matrix
        setInverseVar <- function(solveResult) {
                inverseVar <<- solveResult
        }
        ## return inverse matrix that has been cached
        getInverseVar <- function() {
                inverseVar
        }
        
        ##list function available on the environment
        list(set = setMatrix, get = getMatrix,
             setInverseVar = setInverseVar,
             getInverseVar = getInverseVar)
}


## Write a short comment describing this function
## This function will make an inverse matrix out of passed parameter
## This function assuming that the passed parameter is square matrix and inverseable
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## get inverse matrix of parameter x
        cacheInverseVar <- x$getInverseVar()
        ## check whether it is already exist or not, if logical check is TRUE then return cached inverse
        if(!is.null(cacheInverseVar)) {
                return(cacheInverseVar)
        }
        ## if FALSE then get current normal matrix from x
        cacheMatrix <- x$get()
        
        ## call solve function to create inverse
        solveResult <- solve(cacheMatrix, ...)
        
        ## set the result of inverse process
        x$setInverseVar(solveResult)
        ## return the result
        solveResult
}