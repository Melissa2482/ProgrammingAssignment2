

# This function creates as special matrix to cache the inverse of a matrix. 
# By using this function you can make the lengthy process of this calculation more efficient and less time consuming.
# This first function is simply a list that enables the user to get and set the values of both the matrix and its inverse


makeCacheMatrix <- function(x = matrix ()) {
  inv = NULL
  setinv <- function(y) {
    x <<- y
    y <<- NULL
  }
  get = function()
    x
  setinv = function(inverse)
    inv <<- inverse
  getinv = function()
    inv
  
  list (
    set = set,
    get = get,
    setinv = setinv,
    getinv = getinv
  )
}


#This second function is designed to utilise the objects returned from the first function.
# Using these objects, by calling get() inverse if the object is stored i will return this inverse.
# If no inverse is returned then the function will get the matrix object, solve and return the inverse stored back into the object.

cacheSolve <- function(x, ...) {
  inv = x$getinv()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  matrix.data = x$get()
  inv = solve(matrix.data, ...)
  
  x$setinv(inv)
  
  return(inv)
}