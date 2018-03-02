## Put comments here that give an overall description of what your
## functions do

## This function produces a special "matrix object" consisting of a list that includes a function  
## to set and get the value of a matrix and to set and get the inverse of the matrix itself

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setinv <- function(solve) s <<- solve
  getinv <- function() s
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function computes the inverse of a special matrix object as the output of the "makeCacheMatrix" 
## function above, retrieving the inverse from the cache itself in the case it has been already computed.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  sol <- x$getinv()
  if(!is.null(sol)) {
    message("getting cached data!")
    return(sol)
  }
  data <- x$get()   
  sol <- solve(data, ...)
  x$setinv(sol)
  sol
}
