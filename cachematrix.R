## Overall description of what the functions do
## 1.  `makeCacheMatrix`: This function creates a special "matrix" object
##    that can cache its inverse.
## 2.  `cacheSolve`: This function computes the inverse of the special
##    "matrix" returned by `makeCacheMatrix` above. If the inverse has
##    already been calculated (and the matrix has not changed), then
##    `cacheSolve` retrieves the inverse from the cache.
##  the matrix supplied has to be invertible


## sample test sequence
# source('cachematrix.R')
# mdat <- matrix(c(-1,3,-3, 0,-6,5, -5,-3,1), nrow = 3, ncol = 3)
# mdat
# m<- makeCacheMatrix()
# m$set(mdat)
# m$get()
# cacheSolve(m)
# cacheSolve(m)
# mdat <- matrix(c(-1,3,-3, 0,-6,5, -5,-3,14), nrow = 3, ncol = 3)
# m$set(mdat)
# m$get()
# cacheSolve(m)
## end test


makeCacheMatrix <- function(x = matrix()) {
	## init the inv matrix
	m <- NULL
	
	## Methods
	
		## define set function
		set <- function(y) {
			## cache the matrix
			x <<- y
			## init the inv matrix
			m <<- NULL
		}
		
		## define get function
		get <- function() x
		## set the inv matrix
		setinv <- function(inv) m <<- inv
		## get the inv matrix
		getinv <- function() m
		
	## return a list of functions/primitives/methods for this object	
	list(set = set, get = get,
		setinv = setinv,
		getinv = getinv)


}



cacheSolve <- function(x, ...) {
	## get the inverse from the object
	m <- x$getinv()
	## if not null then return that
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	## else calculate the inverse
	data <- x$get()
	m <- solve(data, ...)
	## store it in the object (cache it)
	x$setinv(m)
	## return it
	m
}