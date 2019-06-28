## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Essentially take the cache mean function written in the example and change 
## the labels etc to make it easier to follow. Use the solve() function as 
## detailed in the documentation.

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      set_inv <- function(inverse) inv <<- inverse
      get_inv <- function() inv
      list(set = set, get = get,
           set_inv = set_inv,
           get_inv = get_inv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      
      inv <- x$get_inv()
      if(!is.null(inv)) {
            message("Getting cached data")
            return(inv)
      }
      
      data <- x$get()
      inv <- solve(data, ...)
      x$set_inv(inv)
      inv
      
}
