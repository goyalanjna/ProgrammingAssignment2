## makeCacheMatrix function creates a special "Matrix" object that can cache its inverse.cacheSolve function computes the inverse 
# of special "Matrix" returned by function makeCacheMatrix. if the inverse for a Matrix has already been calculated then cacheSolve
# will retrieve the inverse from cache.


#This function creates a special "matrix" object that can cache its inverse.

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

## cacheSolve function computes the inverse of special "Matrix" returned by function makeCacheMatrix. 
# if the inverse for a Matrix has already been calculated then cacheSolve will retrieve the inverse from cache.

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


M1_Matrix <- matrix(1:4,nrow = 2)
CC_Matrix <- makeCacheMatrix(M1_Matrix)
cacheSolve(CC_Matrix)
