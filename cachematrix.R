makeCacheMatrix <- function(x = matrix ()) {
  inv = NULL
  setinv <- function(y){
    x <<- y
    y <<- NULL
  }
  get = function()x
  setinv = function(inverse) inv <<- inverse
  getinv = function()inv
  
  list (set = set, get = get, setinv = setinv, getinv = getinv)
}


cacheSolve <- function(x, ...) {
  inv = x$getinv()
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  
  matrix.data = x$get()
  inv = solve(matrix.data, ...)
  
  x$setinv(inv)
  
  return(inv)
}