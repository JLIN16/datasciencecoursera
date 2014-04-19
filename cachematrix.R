## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#Create "special" matrix object from matrix of values; set matrix inverse calculation to NULL (default)
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  #define list of object-related functions
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  #creates "special" object as a list of the 4 functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


#Returns the inverse matrix of a "special" matrix object and calculates value only if not previously cached
cacheSolve <- function(x, ...) {
  #retrieve value of inverse from "special" matrix object
  inverse <- x$getinverse()
  #checks if value already exists in cache
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  #otherwise, retrieve matrix values, calculate inverse, and set value in "special" object
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
