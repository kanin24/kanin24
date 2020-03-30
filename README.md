# kanin24
# Caching the Inverse of a Matrix
# Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matix instead of compute repeatedly
# The illustration below are a pair of functions that are used to create a special object that are use for storing a matrix and caches its inverse
# The function creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
m <- NULL
set <- function(y){
   x <<- y
   m <<- NULL
}
get <- function()x
setInverse <- function(inverse) j <<- inverse
getInverse <- function() m
  list(set = set, get = get,
  setInverse = setInverse,
  getInverse = getInverse)
}

# This function computes the inverse of the special matrix created by makeCacheMatrix above
# If the inverse has already been calculated and the matrix created has not changed, then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
m <- x$getInverse()
if(!is.null(m)){
  message("getting cached data")
  return(m)
}
mat <- x$get()
m <- solve(mat, ...)
x$setInverse(m)
m
}
