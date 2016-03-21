## Caching the Inverse of a Matrix

## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse os a matrix rather than compute it repeatedly.
## To cach the inverse of a matrix I've written a pair of functions.

## The first one, that is, the function below, creates a special matrix object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setInverse <- function(inverse) inv <<- inverse
	getInverse <- function() inv
	list(set = set,
		 get = get,
		 setInverse = setInverse,
		 getInverse = getInverse)
}


## The function below, the second one, computes the inverse of the special matrix
## returned by makeCacheMatrix above. If the inverse has already calculated (and
## the matrix has not changed), then the cacheSolve should retriebe the inverse
## from cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
        	message ("getting cached data")
        	return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}