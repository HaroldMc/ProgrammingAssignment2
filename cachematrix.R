## This is a combination of two functions that can cache the inverse of a matrix
## 

## makeCacheMatrix creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(solve) m <<- solve
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## cacheSolve computes the inverse of the matrix created by makeCacheMatrix
## it it was calculated previously then it pulls the number from the cache
## if it was not calculated previously then it calculates it and puts it in cache

cacheSolve <- function(x=matrix(), ...) {
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
        ## Return a matrix that is the inverse of 'x'
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setmatrix(m)
  m
}
