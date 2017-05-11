# Assignment 2 for "R Programming for Data Science" course

# To verify results, type in the R console, for example, the following:
# SpMatrix <- makeCacheMatrix (matrix(data=runif(100, 0, 20),nrow=10,ncol=10))
# cacheSolve (SpMatrix)
# cacheSolve (SpMatrix)
# The first time the inverse is computed, the second time it is read from cache

makeCacheMatrix <- function(A = matrix()) {
    r <- NULL
    setData <- function(y) {
        A <<- y
        r <<- NULL
    }
    getData <- function() A
    setResultToCache <- function(result) r <<- result
    getResultFromCache <- function() r
    list(setData = setData, getData = getData,
         setResultToCache = setResultToCache,
         getResultFromCache = getResultFromCache)
}


cacheSolve <- function(specialMatrix) {
    result <- specialMatrix$getResultFromCache()
    if(!is.null(result)) {
        message("getting cached data")
        return(result)
    }
    data <- specialMatrix$getData()
    result <- solve(data)
    specialMatrix$setResultToCache(result)
    result
}


