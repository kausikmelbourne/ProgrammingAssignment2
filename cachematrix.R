##  The function makeCacheMatrix return a list of functions to set and 
##  and get the matrix and inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set<-function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(minv) inv <<- minv
  getinv <- function() inv
  list(set = set,
       get = get,
       setinv = setinv,
       getinv = getinv)
}


## The function cacheSolve Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
