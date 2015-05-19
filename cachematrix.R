## Building the functions makeCacheMatrix & cachesolve to store matrices & their reverse in cache
##

## function to cache a matrix and its reverse
makeCacheMatrix <- function(x = numeric()) {
        a <- NULL
        setMatrix <- function(y) {
                x <<- y
                a <<- NULL
        }
        getMatrix <- function() x
        setSolve <- function(solve) a <<- solve
        getSolve <- function() a
        list(setMatrix = setMatrix, getMatrix = getMatrix,
             setSolve = setSolve, getSolve = getSolve)
}


## cacheSolve returns the cached reverse of the matrix if exists
## or calculates it then stores the result in cache
cacheSolve <- function(mkCacheM, ...) {
        svm <- mkCacheM$getSolve()
        if(!is.null(svm)) {
                message("getting cached solved matrix")
                return(svm)
        } else {
                message("not cached reverse matrix")
                blibli <- mkCacheM$getMatrix()
                svm <- solve(blibli)
                mkCacheM$setSolve(svm)
                svm
        }
        ## Returns a matrix that is the inverse of 'x'
}
