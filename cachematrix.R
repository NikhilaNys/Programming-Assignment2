## This exercise is an attempt to making use of the "cached data" on R
## to shorten the time of potentially time consuming operations such
## as calculating an inverse of a matrix


## This function creates a matrix to be used in the rest of the operation

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
	x <<-y
	i <<-NULL
	}
	get <- function() x
	setinverse <- function(inverse) i <<-inverse
	getinverse <- function() i
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)	
}


## This function creates the inverse of the matrix returned by makeCacheMatrix
## above. If the inverse has already been calculated, then the cacheSolve
## retrieves it from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	i <- x$getinverse()
	if(!is.null(i)) {
	message("getting cached data")
	return(i)
	}
	data <- x$get()
	i <- inverse(data, ...)
	x$setinverse(i)
	i
}
