## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly 
## The following two functions are used to cache the inverse of a matrix.


## Function creates a special "matrix" object that can cache its inverse.
## It is really a list containing a functions to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix
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


## Function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## It first checks to see if the inverse of the matrix has already been calculated. 
## If so, it gets the inverse matrix from the cache and skips the computation. 
## Otherwise, it calculates the inverse matrix and sets the value of the inverse matrix in the cache via the setinverse function.
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
