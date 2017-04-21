## We want to cache the inverse of a matrix 
## Below are two functions that cache the inverse of a matrix

## the first function makeCacheMatrix() creates an object 
## that stores a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
   zz <- NULL
  set <- function(y) {
    x <<- y
    zz <<- NULL
  }
  get <- function() x
  # assign value of solve to zz in parent scope
  setsolve <- function(solve) zz <<- solve
  getsolve <- function() zz
  
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## this second function, cacheSolve() requires an argument that is returned 
# by makeCacheMatrix() to retrieve the inverse matrix from the cached value
# that is in the first function's environment 

cacheSolve <- function(x, ...) {
  
  zz <- x$getsolve()
  if(!is.null(zz)) {
    message("getting cached matrix")
    return(zz)
  }
  data <- x$get()
  zz <- solve(data, ...)
  x$setsolve(zz)
  zz

}