## Create an object that contains a matrix
## and caches its inverse
## Use: 
## to create object
## cmatrix <- makeCacheMatrix(matrix)  
## to invert
## inverse <- cacheSolve(cmatrix)

## Create an object that is a matrix caching its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(newinv) inv <<- newinv
  getinv <- function() inv
  
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    inv
  } else {
    data <- x$get()
    inv = solve(data, ...)
    x$setinv(inv)
    inv
  }
}
