## These functions will calculate the inverse of a matrix. They should
## work with even a very large matrix since the first function will 
## help store the matrix in cache.

## This function creates a matrix and a list of functions to help 
## store the matrix in cache. 

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function(y){
		x <<- y
		inverse <<- NULL
	}
	get <- function() x
	setinverse <- function(solve) inverse <<- solve
	getinverse <- function() inverse
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function either retrieves the cached value for the inverse of 
## the matrix or it solves for the inverse of the matrix and stores 
## it in cache using the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inverse <- x$getinverse()
	if(!is.null(inverse)) {
		message("Fetching cached data.")
		return(inverse)
	}
	matrix <- x$get()
	inverse <- solve(matrix, ...)
	x$setinverse(inverse)
	inverse
}
