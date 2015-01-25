## makeCacheMatrix generates a structure to help cache
## the inverse of a matrix calculated by cacheSolve

## Generate a structure for holding a matrix and caching
## it's inverse, new matrix's handed to set clears the
## the cache
## getinverse will be NULL if the inverse isn't caclulated
## yet.
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
       setinverse = setinverse,
       getinverse = getinverse)
}

## Fetch the cached solution if it exists, otherwise
## use solve with one paramter to get the matrix inverse
## then return that solution and cache a copy
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
