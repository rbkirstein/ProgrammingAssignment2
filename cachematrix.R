## This is a pair of functions that cache the inverse of a matrix
## My name is Becky Kirstein

##This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix(), nrow, ncol) {
        i <- NULL
	nrow = nrow
	ncol = ncol
	B <- matrix(x, nrow, ncol)
	set <- function(y) {
	     y = matrix(x, nrow, ncol)
	     B <<- y
	     i <<- NULL
	}
	get <- function() B
	setInverse <- function(inverse) i <<- inverse
	getInverse <- function() i
	list(set=set, get = get,
	     setInverse = setInverse,
	     getInverse = getInverse)

}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been caculated (and
## the matrix has not changed), then the cachesolve should retrieve the
## inverse from the cache.

cacheSolve <- function(B, ...) {
        ## Return a matrix that is the inverse of 'B'
	i <- B$getInverse()
	if (!is.null (i)) {
	     message ("getting cached data")
	     return (i)
	}
	data <- B$get()
	i <- solve(data,...)
	B$setInverse(i)
	i
}
