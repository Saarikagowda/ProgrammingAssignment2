## makeCacheMatrix creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  # Function to set the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL  # Reset cached inverse when matrix changes
  }
  
  # Function to get the matrix
  get <- function() x
  
  # Function to set the inverse
  setinverse <- function(inverse) inv <<- inverse
  
  # Function to get the inverse
  getinverse <- function() inv
  
  # Return list of functions
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## cacheSolve computes the inverse of the matrix returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
  # Try to get cached inverse
  inv <- x$getinverse()
  
  # If inverse is already cached, return it with a message
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # If not cached, compute the inverse
  data <- x$get()
  inv <- solve(data, ...)
  
  # Store the inverse in the cache
  x$setinverse(inv)
  
  # Return the inverse
  return(inv)
}


# Create a matrix
m <- matrix(c(1,2,3,4), nrow=2, ncol=2)

# Create a cached matrix object
cm <- makeCacheMatrix(m)

# Calculate inverse (computes and caches)
cacheSolve(cm)

# Calculate again (retrieves from cache)
cacheSolve(cm)

# Change the matrix
cm$set(matrix(c(2,0,0,2), nrow=2, ncol=2))

# Calculate inverse of new matrix (computes and caches)
cacheSolve(cm)
