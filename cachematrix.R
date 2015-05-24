## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	invmatrix <- NULL
	set <- function(y) {
		x <<- y
		invmatrix <<- NULL
	}
	get <- function() x
	setinverse <- function(inv) invmatrix <<- inv
	getinverse <- function() invmatrix
	list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	invmatrix <- x$getinverse()
	if(!is.null(invmatrix)) {
		message("getting cached data")
		return(invmatrix)
	}
	tempdata <- x$get()
	invmatrix <- solve(tempdata)
	x$setinverse(invmatrix)
	invmatrix
}
