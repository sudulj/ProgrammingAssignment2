## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

## 1. Change mean function name to invMatx (Inverse Matrix)
## 
## ********************************

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinvMatx <- function(invMatx) m <<- invMatx
  getinvMatx <- function() m
  list(set = set, get = get,
       setinvMatx = setinvMatx,
       getinvMatx = getinvMatx)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix 
## has not changed), then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinvMatx()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinvMatx(m)
  m
}
