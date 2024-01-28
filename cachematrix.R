# "makeCacheMatrix" function is used to create a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(matrix) {
    x <<- matrix
    inv <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inverse) {
    inv <<- inverse
  }
  
  getinverse <- function() inv
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# "cacheSolve" function is used to compute the inverse of the special "matrix" and cache the result.
cacheSolve <- function(cacheMatrix, ...) {
  inv <- cacheMatrix$getinverse()
  
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }

  data <- cacheMatrix$get()
  inv <- solve(data, ...)
  
  cacheMatrix$setinverse(inv)
  
  inv
}

# Example:
my_matrix <- makeCacheMatrix (matrix(c(1,2,3,4), nrow = 2))
cacheSolve(my_matrix)
