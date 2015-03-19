## Assignment: Caching the Inverse of a Matrix
## 
## The assignment is to write a pair of functions that cache the inverse of a 
## matrix.

## This function creates a special "matrix" object that can cache its inverse.
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
   inverse <- NULL
   set <- function(y) {
      x <<- y
      inverse <<- NULL
   }
   get <- function() return(x)
   set_inverse <- function(inv) inverse <<- inv
   get_inverse <- function() return(inverse)
   return(list( 
      set = set, 
      get = get, 
      set_inverse = set_inverse, 
      get_inverse = get_inverse))
}


## This function computes the inverse of the special "matrix" returned by 
## `makeCacheMatrix` above.If the inverse has already been calculated (and the 
## matrix has not changed), then it retrieves the inverse from the cache.  
cacheSolve <- function(x, ...) {
   inverse <- x$get_inverse()
   if(!is.null(inverse)) {
      message("Getting cached data.")
      return(inverse)
   }
   data <- x$get()
   inverse <- solve(data, ...)
   x$set_inverse(inverse)
   return(inverse)
}
