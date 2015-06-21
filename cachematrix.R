## The functions create a cached matrix and solves 
## computes the inverse of the caches matrix

## Creates a mtrix stored in cache

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- matrix(y)
    m <<- NULL
  }
  get <- function() x
  setMatrixInverse <- function(solve) m <<- inverse
  getMatrixInverse <- function() m
  list(set = set, get = get,
       setMatrixInverse = setMatrixInverse,
       getMatrixInverse = getMatrixInverse)
  
}


## Computes the inverse of the cached matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getMatrixInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$getMatrixInverse()
  m <- solve(data, ...)
  x$setMatrixInverse(m)
  m
}
