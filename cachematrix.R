## This function creates a cache environment where a matrix
## and its inverse can be stored to save computation time
makeCacheMatrix <- function(mat = matrix()) {
  inverse <- NULL
  set <- function(y) {
    mat <<- y
    inverse <<- NULL
  }
  get <- function() mat
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function can interact with the inverse matrix cache
## It retrieves the cached inverse matrix or compute it and store it
## if there is no a pre-computed inverse matrix inside the cache
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinverse(inv)
  inv
}
