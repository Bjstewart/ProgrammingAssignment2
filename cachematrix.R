## The makeCacheMatrix function creates a matrix and sets the value of its inverse.  The cacheSolve function checks to see
## if a matrix has been previously inverted and solves its inverse if it has not.  

## The makeCacheMatrix function creates a matrix
makeCacheMatrix <- function(x = matrix()) {
  Inv <- NULL
  set <- function(y) {
    x <<- y
    Inv <<- NULL
  }
  get <- function() x
  setInv <- function(solve) Inv <<- solve
  getInv <- function() Inv
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}

## The cacheSolve function inverts a matrix
cacheSolve <- function(x, ...) {
  Inv <- x$getInv()
  if(!is.null(Inv)) {
    message("getting cached data")
    return(Inv)
  }
  data <- x$get()
  Inv <- solve(data, ...)
  x$setInv(Inv)
  Inv
}
