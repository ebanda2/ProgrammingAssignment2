## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
