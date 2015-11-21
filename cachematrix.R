## The functions below calculate the inverse matrix and stores the result in cache.
## When the function calculates checks if the inverse have been calculated and stored
## previously.

## Matrix Set Up
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse
## 4.  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  iM <- NULL
  set <- function(y) {
    x <<- y
    iM <<- NULL
  }
  get <- function() x
  setInverse <- function(i) iM <<- i
  getInverse <- function() iM
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Return the inverse matrix. If the matrix has been calculated before, returns the cache value
## avoiding make a new calculation

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
