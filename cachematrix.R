## These functions check if an inverted matrix is in the cache.
## If a matrix is not in the cache, it will calculate the inverse
## of the given matrix.

## This function creates a cached matrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  } 
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse, getinverse = getinverse)
}


## This function calculates the inverse of a matrix or 
## returns the cached value if it's available.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
