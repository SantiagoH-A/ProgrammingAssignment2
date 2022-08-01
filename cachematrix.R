## The function makeCacheMatrix creates a specific matrix which
## is able to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  n <- NULL
  set <- function(y) {
    x <<- y
    n <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) n <<- inverse
  getinverse <- function() n
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## The function cacheSolve returns the inverse of the matrix
## returned by our first function, makeCacheMatrix. If the inverse
## has already been calculated, this function will return its inverse
## from the cache.

cacheSolve <- function(x, ...) {
  n <- x$getinverse()
  if(!is.null(n)) {
    return(n)
  }
  matrix <- x$get()
  n <- solve(matrix, ...)
  x$setinverse(n)
  n
}

