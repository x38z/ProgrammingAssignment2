

## create a function which accepts a null matrix argument
makeCacheMatrix  <- function(x = matrix()) {
## initialize the value of the matrix inverse to NULL locally.
  s <- NULL
  ##sets the value of inverse matrix to null globally.
  ##sets the value of x globally.
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  ## returns x
  get <- function() x
  ## sets inverse matrix to global cache.
  setinv <- function(solved) s <<- solved
  ## gets inverse matrix from global cache.
  getinv <- function() s
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## Return a matrix that is the inverse of 'x'
cachesolve <- function(x, ...) {
  s <- x$getinv()
   #if the inverse matrix exists, it gets it.
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setinv(s)
  s
}
