## Shuping Peng
## The pair of functions is able to cache the inverse of a matrix

## Function makeCacheMatrix creates a special "matrix" object that can cache the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  # set the matrix and create a null matrix for the inverse of the matrix
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  # get the matrix
  get <- function() x
  
  # set the inverse of the matrix
  setinverse <- function(inverse_matrix) inverse <<- inverse_matrix
  
  # get the inverse of the matrix
  getinverse <- function() inverse
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  # If the inverse already exists, the following code will get it from the cache
  inverse <- x$getinverse()
  if(!is.null(inverse) && x == x$get()) {
    message("getting cached matrix")
    return(inverse)
  }
  # If the inverse does not exist, the following code will first calculate it and then cache it
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
















