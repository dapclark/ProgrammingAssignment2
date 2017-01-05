## These two functions work together to create a matrix, invert it, and then store the value of the inversion in a cache.  If asked to re-calculate the inversion, the functions instead return the cached value.

## makeCacheMatrix creates a list of functions that serve as the input for cacheSolve.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  ## Sets x and inv in the parent environment.
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## Defines the rest of the getters and setters.
  get <- function() x
  set_inv <- function(inverse) inv <<- inverse
  get_inv <- function() inv
  
  ## Creates the new list object.
  list(set = set, get = get, set_inv = set_inv, get_inv = get_inv)
  
}

## cacheSolve takes the output of makeCacheSolve and either 1) calculates and returns the inverse of the matrix or, if the inverse has already been calculated, 2) returns the cached value.
cacheSolve <- function(x, ...) {
  inv <- x$get_inv()

  ## If the inverse of the matrix has already been calculated (inv exists), simply return inv.
  if(!is.null(inv)) {
    message("Getting cached data.")
    return(inv)
  }
  
  ## Otherwise, calculate the inverse.
  data <- x$get()
  inv <- solve(data, ...)
  
  ## Then, set the cache to equal the inverse of the matrix and return inv.
  x$set_inv(inv)
  return (inv)
}