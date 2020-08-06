## Assignment: Caching the Inverse of a Matrix
## Matrix inversion is usually a costly computation 
## and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Next, there are two functions, the first creates a special "matrix" object that can cache its inverse
## the second computes the inverse of the special "matrix" returned by makeCacheMatrix.

## makeCacheMatrix is a function that creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x= matrix()){
		matrixinverse <- NULL
		set <- function (y) {
			x <<- y
			matrixinverse <<- NULL
		}
		get <- function()x
		setmatrixinverse <- function(inverse) matrixinverse <<- inverse
		getmatrixinverse <- function () matrixinverse
		list(set = set,
			get = get,
			setmatrixinverse = setmatrixinverse,
			getmatrixinverse = getmatrixinverse)
}

## cacheSolve is a function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function (x,...){
		matrixinverse <- x$getmatrixinverse()
		if(!is.null(matrixinverse)){
			message("getting cached data")
			return(matrixinverse)
		}
		data <- x$get()
		matrixinverse <- solve(data,...)
		x$setmatrixinverse(matrixinverse)
		matrixinverse
}
