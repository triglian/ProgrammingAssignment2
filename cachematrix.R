## This is the submission for Programming Assignment 2 of R Programming at Coursera
## The purpose of this exercise is to cache the inverse of matrices to speed up computations
## There are two functions implemented here: `makeCacheMatrix` and `cacheSolve`. The firs
## implements a new version of matrix with cacheable inverse and the second is the function to call
## when we want the inverse of the array.

## Returns a special `matrix` that holds a reference to its inverse

makeCacheMatrix <- function(x = matrix()) {
  # initialize inverse to NULL
  i <- NULL
  
  # setter function for the actual matrix
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  # getter function for the actual matrix
  get <- function() x
  
  # sets the inverse
  setsolve <- function(inv) i <<- inv
  
  #gets the inverse
  getsolve <- function() i
  
  # return list with the augmented matrix
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Calculates (if necessary) and returns the inverse of matrices created with the 'makeCacheMatrix' function 

cacheSolve <- function(x, ...) {
  # try cache
  i<- x$getsolve()
  
  # if cached return inverse immediately and exit
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  # inverse is null so we have to calculate it
  data <- x$get()
  i <- solve(data, ...)
  x$setsolve(i)
  
  ## Return a matrix that is the inverse of 'x'
  i
}
