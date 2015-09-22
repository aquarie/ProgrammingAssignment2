## Using an approach from an example of function from instructions to the Assignment 2
## we create an object ("matrix") to store a matrix itself and cache its invese.
## First we make "matrix" object able to cache the inverse of matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}




## As required in the assignment, the function computes the inverse of the "matrix"  
## created by makeCacheMatrix function. The function also should retrieve the inverse   
## from the cache when the inferse was calculated.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
