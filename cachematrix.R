## The pair of functions is used to cache the inverse of a matrix object.  The 
## "makeCacheMatrix" function creates a list of functions to set the retrieve
## the matrix and its inverse.  The "cacheSolve" function determines if the
## matrix inverse is cached and, if so, returns that value; otherwise, it uses
## the solve function to compute the inverse and call the function to cache the
## inverse matrix.

## 'makeCacheMatrix' creates the functions to cache the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
    }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## 'cacheSolve' computes the inverse of the special matrix.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
