## Author: Albert Santos
## This file contains functions that inverses a square matrix 
## and saves it in cache for future use

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(a_matrix = matrix()) {
  i_matrix <- NULL
  
  ##set the matrix
  set <- function(set_matrix) {
    a_matrix <<- set_matrix
    ## set inverse to NULL in case user set a new matrix
    i_matrix <<- NULL
  }
  
  ## retrieve the matrix
  get <- function() a_matrix
  
  ## get/set the inverse of the matrix
  setinverse <- function(inverse_m) i_matrix <<- inverse_m
  getinverse <- function() i_matrix
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix"
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cached version of the inverse matrix will be used instead

cacheSolve <- function(a_matrix, ...) {
  ## Return a matrix that is the inverse of 'a_matrix'

  ## check cache
  i_matrix <- a_matrix$getinverse()
  if(!is.null(i_matrix)) {
    message("getting cached data")
    return(i_matrix)
  }
  
  ## compute for matrix inverse
  data_matrix <- a_matrix$get()
  i_matrix <- solve(data_matrix)
  a_matrix$setinverse(i_matrix)
  
  ## return inverse
  i_matrix
  
}
