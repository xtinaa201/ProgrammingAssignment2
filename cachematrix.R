## makeCacheMatrix is a function that creates a special "matrix" object that
##can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  #Initialize inverse property
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  #Method to get matrix
  get <- function() {
    x
  }
  
  #Method to set the inverse of matrix
  setInverse <- function(inverse) {
    inv <<- inverse
  }
  
  #Method to get the inverse of matrix
  getInverse <- function() {
    inv
  }
  
  #Return list of methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



## cacheSolve function computes the inverse of the special "matrix" returned by 
##makeCacheMatrix above. If the inverse has already been calculated (and matrix
## has not changed), then the cachesolve should retrieve the inverse from the
##cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  #Get matrix from object
  data <- x$get()
  
  #Calculate inverse
  inv <- solve(data, ...)
  
  #Set the inverse to the object
  x$setInverse(inv)
  inv
}
