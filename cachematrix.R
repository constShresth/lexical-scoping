
## The makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
## It includes methods to set and get the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  # Initialize a variable to store the inverse
  inv <- NULL
  
  # Method to set the matrix and reset the cached inverse
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # Method to get the matrix
  get <- function() x
  
  # Method to set the inverse of the matrix
  setInverse <- function(inverse) inv <<- inverse
  
  # Method to get the inverse of the matrix
  getInverse <- function() inv
  
  # Return a list of the methods
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## The cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated and cached, it retrieves the inverse from the cache to avoid recomputation.
## Otherwise, it computes the inverse, caches it, and then returns it.

cacheSolve <- function(x, ...) {
  # Retrieve the cached inverse if it exists
  inv <- x$getInverse()
  
  # If the inverse is already cached, return it
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # Get the matrix and compute its inverse if not cached
  mat <- x$get()
  inv <- solve(mat, ...)
  
  # Cache the computed inverse for future use
  x$setInverse(inv)
  
  # Return the inverse
  inv
}
