## These functions calculate, cache, and get the inverse of any invertable matrix
## Using them together may save time relative to calculating the inverse repeatedly

## this function creates a list of functions that 'set' and 'get' the specified matrix,
## 'setinverse' and 'getinverse' set and get the resulting inverted matrix

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


## This function returns the inverted matrix, checking for a cached result first

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i  ## Return a matrix that is the inverse of 'x'
}
