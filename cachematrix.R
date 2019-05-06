## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.


## cacheSolve: This function computes the inverse of the special 
## matrix returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
   x <<- y
    inv <<- NULL
}
   get <- function() x
   setInverse <- function(inverse) inv <<- solve(inv)
  getInverse <- function() inv
  list (set = set,get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
   if(!is.null(inv)) {
   message("getting cached data")
  return(inv)
 }
 mat <- x$get()
 inv <- solve(mat, ...)
x$setInverse(inv)
inv
}
