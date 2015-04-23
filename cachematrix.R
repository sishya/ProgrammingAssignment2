## Put comments here that give an overall description of what your
## functions do

## Makes a special "matrix" that allows cached values of its inverse to be used

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y)
	{
		x <<-y
		i <<- NULL
	}
	get <- function() x
	setinv <- function(inv) i<<-inv
	getinv <- function() i
	list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## This function computes inverse or returns cached value of inverse.  Should be run after makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i

}
