## The following functions can be used to create a matrix that can cache
## it's inverse. It assumes that the matrix created is inversible.
##
## USAGE: 
##    x <- makeCacheMatrix(inversibleMatrix)
##    y <- cacheSolve(x)
##
## If cacheSolve is called more than once in a session, the cached version of the matrix inverse
## will be returned, saving the overhead of calculating the inverse each time.
##


## PURPPOSE: Establishes the global variables and functions required to access the cached instance of the 
## inverse of the created inversible matrix, x.
##
## The returned list gives the created matrix access to the internal functions that cache the inverse of the matrix. 
## These functions are used by the "cacheSolve" function to initialize the cached global variables.
##
makeCacheMatrix <- function(x = matrix()) {
  ##Placeholder for inverse results
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() { x }
  setinverse <- function(inverse) { i <<- inverse }
  getinverse <- function() { i }
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## PURPOSE: Return a matrix that is the inverse of 'x'
##
## Will return a cached version if it has already been computed in the session
## The parameter x should be a matrix that has been created by the "makeCacheMatrix" function above
##
cacheSolve <- function(x, ...) {
  
  i <- x$getinverse()
  
  
  if(!is.null(i)){
    message("getting cached data...")
    return(i)
  }
  
  data <- x$get()
  i <- solve(data)
  
  x$setinverse(i)
  return(i) 
  
}
