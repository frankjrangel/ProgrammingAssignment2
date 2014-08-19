## This file contains two functions, 
## makeCacheMatrix creates the matrix and caches its inverse
## cacheSolve computes the inverse of a matrix but it checks first if
## it has been calculated before, if so, it uses the cached value

## This function makes a matrix and sets the inv variable, used to compute the inverse, to NULL

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	list(
		set = set,
		get = get,
		setinverse = setinverse,
		getinverse = getinverse
	)
}


## This function computes the inverse of a matrix only if it hasn't been computed before, otherwise it gets the cached value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	
	inv <- x$getinverse()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinverse(inv)
	inv
}
