## These functions create a special matrix "makeCacheMatrix" and caches the matrix inverse
## Then we create a loop function "CacheSolve" to compute the inverse of the special matrix function we created in "makeCacheMatrix"

## The "makeCacheMatrix" is a function that creates a special 'matrix'to cache its inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(solveMatrix) inv <<- solveMatrix
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## "cacheSolve" is a function created to compute the inverse of the matrix "makeCacheMatrix" created above

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv
}
