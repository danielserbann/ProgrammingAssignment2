# makeCacheMatrix function creates a special "matrix"
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  # set the value of the matrix   
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  # get the value of the matrix
  get <- function() x
  # set the value of the inverse
  setinverse <- function(solve) inverse <<- solve
  # get the value of the inverse
  getinverse <- function() inverse
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

# cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above
cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  # checks to see if the inverse has already been calculated and if it exists than recover it
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  # compute the inverse if it is not already computed
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}