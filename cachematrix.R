## Functions for Caching the Inverse of a Matrix

## makeCacheMatrix is used to create a matrix that can cache its inverse
## It can:
## 1. get the value of the matrix
## 2. set the value of the matrix
## 3. set the matrix inverse
## 4. get the matrix inverse

makeCacheMatrix <- function(x = matrix()) {

	## Initialising the inverse and the function to set the value of the matrix
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	
	## Functions to get matrix, and get and set inverse
	get <- function() x
	setinverse <- function(solve) i <<- solve
	getinverse <- function() i
	
	## List of matrix functions
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)

}


## cacheSolve calculates the inverse of a square matrix and caches the result
## If the inverse has already been calculated, a chached version is returned

cacheSolve <- function(x, ...) {
	
	## Get the inverse of the matrix
	## First check if there is a cached version, if so return that
	i <- x$getinverse()
	if(!is.null(i)) {
		message("getting cached data")
		return(i)
	}
	
	## If there is no cached version, get the matrix, run the solve function,
	## and store the cached version. Return the inverse i
	data <- x$get()
	i <- solve(data, ...)
	x$setinverse(i)
	i

}