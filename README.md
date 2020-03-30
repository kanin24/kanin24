# kanin24
# Caching the Inverse of a Matrix
# Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matix instead of compute repeatedly
# The illustration below are a pair of functions that are used to create a special object that are use for storing a matrix and caches its inverse
# The function creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
j <- NULL
set <- function(y){
   x <<- y
   j <<- NULL
}
get <- function()x
setInverse <- function(inverse) j <<- inverse
getInverse <- function() j
  list(set = set, get = get,
  setInverse = setInverse,
  getInverse = getInverse)
}

# This function computes the inverse of the special matrix created by makeCacheMatrix above
# If the inverse has already been calculated and the matrix created has not changed, then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
j <- x$getInverse()
if(!is.null(j)){
  message("getting cached data")
  return(j)
}
mat <- x$get()
j <- solve(mat, ...)
x$setInverse(j)
j
}
