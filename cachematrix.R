## This code consists of two functions that cache the inverse of a matrix.


## makeCacheMatrix creates a special matrix object that can cache its inverse value as returned by the
## R solve function. 
makeCacheMatrix <- function(x = matrix()) {
  matr_inv <- NULL    # set matr_inv to NULL so we don't get an error when calling getinverse the first time.
  set <- function(y) {
    x <<- y        # set x of the parent environment to the argument passed to makeCacheMatrix
    matr_inv <<- NULL # set matr_inv of the parent environment to NULL.
  }
  get <- function() x # returns x from its parent function. it's actually the x that was set 
  # by function set, which is the matrix passed to makeCacheMatrix
  setinverse <- function(inverse) matr_inv <<- inverse # set matr_inv from the parent environment
  # to the value that was passed to this
  # function.
  getinverse <- function() matr_inv # return matr_inv from the parent function.
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve expects a special argument x passed as input, which is a special matrix
## created by the makeCacheMatrix function.
## It then checks whether the inverse of that matrix is already cached, and if it is, returns it.
## If it is not cached, it calculates the inverse, stores it as matr_inv and returns it.
cacheSolve <- function(x, ...) {
  matr_inv <- x$getinverse() # check if we already calculated the inverse. if yes, retrieve it.
  if(!is.null(matr_inv)) {
    # if we already calculated the inverse, return it.
    return(matr_inv)
  } else {
    # call the solve function to calculate the matrix inverse. the arguments passed to solve
    # are the matrix initially passed to makeCacheMatrix, plus additional parameters passed to
    # cacheSolve.
    matr_inv <- solve(x$get(), ...)
    # cache the matrix inverse, and return it.
    x$setinverse(matr_inv)
    return(matr_inv)
  }
}
