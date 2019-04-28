#' Creates a matrix cache, a list of functions to a matrix and an inverse.
#' @param mtrx matrix object
#' @return list of accessors and mutators to mtrx and an inverse
makeCacheMatrix <- function(mtrx = matrix()) {
	invMtrx <- NULL
	set <- function(otherMatrix) {
		mtrx <<- otherMatrix
		invMtrx <<- NULL
	}
	get <- function() mtrx
	setinv <- function(inverted) invMtrx <<- inverted
	getinv <- function() invMtrx
	list(set = set, get = get,
			 setinv = setinv,
			 getinv = getinv)
}

#' Computes the inverse of a matrix provided in a matrix cache, if such
#' value is not avalible in the cache. If this the case, the inverse will be
#' cached in the object and will be provided in later calls to this function.
#' @param mtrxCache is a matrix cache accessible via accessors and mutators
#' @return the inverse of the matrix cached in mtrcCache
#' #' @examples
#' > mymtrx = matrix(c(2,0,0,2), nrow=2, ncol = 2)
#' > cachedMtrx <- makeCacheMatrix(mymtrx)
#' > cacheSolve(cachedMtrx)
#' [,1] [,2]
#' [1,]  0.5  0.0
#' [2,]  0.0  0.5
cacheSolve <- function(mtrxCache, ...) {
    invMtrx <- mtrxCache$getinv()
    if(!is.null(invMtrx)) {
        message("getting cached inverse matrix")
        return(invMtrx)
    }
    cachedMatrix <- mtrxCache$get()
    invMtrx <- solve(cachedMatrix, ...)
    mtrxCache$setinv(invMtrx)
    invMtrx
}
