## Put comments here that give an overall description of what your
## functions do

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


## Write a short comment describing this function

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
