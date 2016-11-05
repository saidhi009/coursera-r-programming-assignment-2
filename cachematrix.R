## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly. 
## Thus the two functions are written which are described below

## makeCacheMatrix is function that takes matrix as an argument. It's functions are:
##set the value of the matrix
##get the value of the matrix
##set the value of the inverse
##get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(i) inv <<- i
    getinverse <- function() inv
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


##The following function calculates the inverse of the special matrix created with the above function. 
##However, it first checks to see if the inverse has already been calculated. 
##If so, it gets the inverse from the cache and skips the computation. 
##Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setmean function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		
	inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting inverse")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
