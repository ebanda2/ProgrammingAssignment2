## This function creates a "matrix" object that can 
## cache its inverse.
##
makeCacheMatrix <- function(x = matrix()) { 
  inverseMatrix <- NULL
  set <- function(new_matrix) {
    x <<- new_matrix
    inverseMatrix <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inverseMatrix <<- inverse
  getinverse <- function() inverseMatrix
  list(set = set, get = get,
     setinverse = setinverse,
     getinverse = getinverse)
}


## this function computes the inverse of the "matrix" 
## returned by makeCacheMatrix above. If the inverse has already
## been calculated (and the matrix has not changed), then the
## cachesolve should retrieve the inverse from the cache.
##
## 
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  matrix <- x$get()
  inverseMatrix <- solve(matrix, ...)
  x$setinverse(inverseMatrix)
  inverseMatrix
}
