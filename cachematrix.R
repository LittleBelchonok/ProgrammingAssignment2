## This file contains two functions that calculate, cache and restore the 
## inverse of a given matrix

## This function returns a list of auxiliary functions that cache and restore 
## inverse matrix

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setInverse <- function(inverse) inv <<- inverse
	getInverse <- function() inv
	list (
		set = set,
		get = get,
		setInverse = setInverse,
		getInverse = getInverse
	)
}


## This function checks whether inverse matrix is cached, caculates and 
## caches it if necessary, returns inverse matrix

cacheSolve <- function(x, ...) {
	inv <- x$getInverse()
	if ( !is.null(inv) ) {
		message("Getting cached data")
		return(inv)
	}
	inv <- solve(x$get(), ...)
	x$setInverse(inv)
	return(inv)
}
