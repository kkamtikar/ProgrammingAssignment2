## This file contains a pair of functions - makeCacheMatrix and cacheSolve that allow caching of inverse of a matrix.

## makeCacheMatrix creates a matrix object that can cache its inverse.
## It consists of a list containing four functions-
## get, set, setinverse, and getinverse

makeCacheMatrix <- function(x = matrix()) {
	
	inv <- NULL
	set <- function(y) {
		
		## "<<-" is an operator that allows to assign value to an object that already exists in a different environment/ "parent" context.
		x <<- y
		inv <<- NULL
	}

	get <- function() {
		x
	}
	
	setinverse <- function (inverse) {
		inv <<- inverse
	}

	getinverse <- function() {
		inv
	}
	
	list (get = get, set = set, setinverse = setinverse, getinverse = getinverse )

	}

	
	
## cacheSolve gets the inverse of the matrix and if the inverse is already calculated and has not changed then returns the inverse from cache.
## If the inverse for matrix is not cached, it is computed using solve function.

cacheSolve <- function(x, ...) {

	inv <- x$getinverse()

	## Check if inv returned is null
	## If inv is not null, then return the existing "cached" value
	if(!is.null(inv)) {
		message("Getting cached data.")
		return (inv)
	}
	
	## if inv is null, it means it is not cached already. So calculate inverse using solve function.
	data <- x$get()
	inv <- solve(data, ...)
	x$setinverse(inv)
	inv

}
