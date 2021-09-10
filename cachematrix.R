## functions can 1) create a "matrix" object that can cache its inverse
## and 2) compute the inverse of the "matrix" object, circumventing the 
## potentially intensive calculation if the inverse has already been calculated

## creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	## inverse variable: inv ; initialized to null
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setInverse <- function(inverse) inv <<- inverse
	getInverse <- function() inv
	list( set = set,
		get = get,
		setInverse = setInverse,
		getInverse = getInverse)
}


## computes the inverse of the special "matrix" returned by makeCacheMatrix
## if the inverse has already been calculated and the matrix is unchanged,
## then this function retrieves the inverse from the cache

## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
	## use getter defined in above function	
	inv <- x$getInverse()
	
	## if inverse in cache, return previously calculated inverse
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	## else calculate inversed matrix, set and return
	mat <- x$get()
	inv <- solve(mat, ...)
	x$setInverse(inv)
	inv
}
