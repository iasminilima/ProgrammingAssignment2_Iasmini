## Put comments here that give an overall description of what your
## functions do


## Write a short comment describing this function
## The "makeCacheMatrix" function creates a special array object, 
## and then the "cacheSolve" function calculates the inverse of the array. 
## If the inverse of the array has already been computed, it is found in the cache and returns it, 
## with no need to recalculate it.

makeCacheMatrix <- function(x = matrix()) {
  
  inverse_x <- NULL
  set <- function(y) {
    x <<- y
    inverse_x <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inverse_x <<- inverse
  getinverse <- function() inverse_x
  list(set = set,
       get = get,
       setinverse  = setinverse ,
       getinverse = getinverse)
  
}		  

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  inverse_x <- x$getinverse()
  if(!is.null(inverse_x)) {
    message("Getting cached data.")
    return(inverse_x)
  }
  data <- x$get()
  inverse_x <- solve(data)
  x$setinverse(inverse_x)
  inverse_x
}
