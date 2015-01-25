## 
## These two functions create and manipulate matrices.


## This is a special matrix object which is actually a list of functions to set and
## get a matrix.  Also, it can also cache the inverse of the function.

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(newInv) inv <<- newInv
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse,
       getInverse = getInverse)
  
}


## This function will return the inverse of a function but will first check
## the cache to see if the data already exists and return that if it does.
## If the data is not cached already, it will calculate the inverse and set,
## the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

  invFunc <- x$getInverse()
  if(!is.null(invFunc)) {
    message("getting cached data")
    return(invFunc)
  }
  data <- x$get()
  invFunc <- solve(data)
  x$setInverse(invFunc)
  invFunc
}
