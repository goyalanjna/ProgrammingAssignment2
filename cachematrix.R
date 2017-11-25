## makeCacheMatrix function creates a special "Matrix" object that can cache its inverse.cacheSolve function computes the inverse 
# of special "Matrix" returned by function makeCacheMatrix. if the inverse for a Matrix has already been calculated then cacheSolve
# will retrieve the inverse from cache.


#This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

     i <- NULL
     set <- function(y) {
         x <<- y
         i <<- NULL
     }
     get <- function() x
     setmatrix <- function(inverse) i <<- inverse
     getmatrix <- function() i
     list(set = set, get = get,
          setmatrix = setmatrix,
          getmatrix = getmatrix)
 }

## cacheSolve function computes the inverse of special "Matrix" returned by function makeCacheMatrix. 
# if the inverse for a Matrix has already been calculated then cacheSolve will retrieve the inverse from cache.

cacheSolve <- function(x, ...) {
 i <- x$getmatrix()
     if(!is.null(i)) {
         message("getting cached data")
         return(i)
     }
     data <- x$get()
     i <- solve(data, ...)
     x$setmatrix(i)
 i
 }


M1_Matrix <- matrix(1:4,nrow = 2)
CC_Matrix <- makeCacheMatrix(M1_Matrix)
cache_Solve(CC_Matrix)
