## Put comments here that give an overall description of what your
## functions do

## Creates a list (or 'matrix') containing a function that will cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## Calculates the inverse of the previously created matrix 
## If invs is already calculated, this function will retrieve it

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

## testing the functions
mymatrix$set(matrix(c(2, 2, 1, 4), 2, 2))
mymatrix$get()
mymatrix$getinverse()
cacheSolve(mymatrix)
mymatrix$getinverse()
