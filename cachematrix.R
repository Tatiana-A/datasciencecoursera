## Function 1 creates a matrix and puts previous results to NULL
## Function 2 checks if a cached value of the inverted matrix exists(not a NULL), 
## otherwise calculates it.


makeCacheMatrix <- function(x = matrix()) {
  m<- NULL
  set<- function (y) {
    x<<- y
    m<<- NULL
  }
  get <- function() {
    return(x)
  }
  setsolve<- function(solve){
    m<<- solve    
  }
  getsolve<- function(){
    return(m)
  }
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}

## This function sets m to NULL and x to y, then m is set to solve and returned. 
## In the end - list of functions to be called from outside.

cacheSolve <- function(x, ...) {
  m<- x$getsolve()
  if (!is.null(m)){
    message("getting cached data")
    return(m)
  }else{
    data<- x$get()
    m<- solve(data,...)
    x$setsolve(m)
    return(m)
  }
}
## Return a matrix that is the inverse of 'x'

