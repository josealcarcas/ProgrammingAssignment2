## Create a special "matrix", wich is really a list containing
## a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the matrix inverse
## 4. get the value of the matrix inverse
makeCacheMatrix <- function(x = matrix()) {
  cachedInverse <- NULL
  # Function that stores 'y' matrix in 'x' and set 'cachedInverse' to null.
  set <- function(y) {
    x <<- y
    cachedInverse <<- NULL
  }
  # Function that get 'x' cached matrix.
  get <- function() x
  # Function that stores 'inverse' value in 'cachedInverse'.
  setInverse <- function(inverse) cachedInverse <<- inverse
  # Function that get the value stored in 'cachedInverse'.
  getInverse <- function() cachedInverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Return a matrix that is the inverse of 'x'.
## If inverse is cached we get its value.
## It it's not cached, we calculate it.
cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setInverse(inverse)
  inverse
}