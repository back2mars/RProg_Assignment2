## cachematrix.R: Caching the Inverse of a Matrix

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	## initialization
	inv_matrix <- NULL
	## set the value of the matrix
	set <- function(y) {
		x <<- y
		inv_matrix <<- NULL
	}
	## get the value of the matrix
	get <- function() x
	## set the value of the inverse matrix
	setinv <- function(inverse) inv_matrix <<- inverse
	## get the value of the inverse matrix
	getinv <- function() inv_matrix
	list (set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
	inv_matrix <- x$getinv()
	## check if the inverse matrix exists
	if (!is.null(inv_matrix)) {
		## if so, retrieve the inverse matrix from cache
		message("getting cached data")
		return(inv_matrix)
	}
	## if not, get the inverse matrix
	data <- x$get()
	inv_matrix <- solve(data)
	x$setinv(inv_matrix)
        ## Return a matrix that is the inverse of 'x'
	inv_matrix
}
