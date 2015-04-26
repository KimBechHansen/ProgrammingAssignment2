## makeCacheMatrix takes a matrix object as a parameter
## cacheSolve inverts the matrix and caches the result

makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set = function(y) {
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Inverts the matrix in the makeCacheMatrix function and stores it in the cache

cacheSolve <- function(x, ...) {
## Return the inverse of 'x'
  
  inv = x$getinv()
  
  # if the inverse exists
  if (!is.null(inv)){
    # Return cached value. 
    message("Returning the cached value")
    return(inv)
  }
  
  # ...else calculate and store in cache
  matrix.val = x$get()
  inv = solve(matrix.val, ...)
  
  # Put the inverse in cache with the setinv function.
  x$setinv(inv)
  
  return(inv)
}