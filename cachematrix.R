## These functions compute the inverse of a matrix and cache the inverse,
## if it hasn't been done before.
## If the inverse has been computed, the functions retrieve the inverse
## from the cache so the expensive computation wouldn't have to be done 
## again.


## The function `makeCacheMatrix`` creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## the function `cacheSolve` computes the inverse of the matrix
##  returned by `makeCacheMatrix`.
## If the inverse has already been calculated 
## (and the matrix has not changed), then
##  `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}


#test
m1 <- matrix(c(1,3,-5,6,-3,2,8,12,23), nrow = 3, ncol = 3)
ma1 <- makeCacheMatrix(m1)
m1
solve(m1)
cacheSolve(ma1)
