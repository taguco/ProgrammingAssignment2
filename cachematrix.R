
## This function creates a list of 4 functions: the one that calls 
## the matrix to obtain its invert, the one that gets the value of the
## matrix, the one that sets the function solve, which inverts a matrix,
## and the one that assigns the value of the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  
  ## Let us state that the cached values "c" are null.
  
  c <- NULL
  
  ## The function that assigns the matrix to "set", from an environment
  ## different from the function environment.
  
  set <- function(y) {
    x <<- y
    c <<- NULL
  }
  
  ## The function that returns the value of the matrix to "get"
  
  get <- function() x
  
  ## The function that sets the solve function to "setsolve"
  
  setsolve <- function(solve) c <<- solve
  
  ## The function that gets the result of the solve function to "getsolve"
  
  getsolve <- function() c
  
  ## Finally, the list is completed.
  
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## This function, applied to the list of the previous function, retrieves
## the value of the inverted matrix, checking if the inverse has been already
## calculated. If so, it is catched from the cache memory, skiping the computation.

cacheSolve <- function(x, ...) {
  
  ## "m" is assigned the exit value of the getsolve function.
  
  m <- x$getsolve()
  
  ## Now, it is checked if the "m" value is null. If not, it means that 
  ## its value has been previously computed and stored in the cache. Hence,
  ## its value is set as the exit value of the function, and the computing
  ## is stopped, finisthing the cacheSolve function.
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## If there is no previous value stored in the cache for m, it is 
  ## computed and recorded in "m". It is retrieved as well.
  
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  
  ## solved matrix is returned
  
  m
}

## In order to run the code, 'cacheSolve(makeCacheMatrix(x))' should be written.
## Or 'y <- makeCacheMatrix(x)' 'cacheSolve(y)'. By running the last option,
## it is possible to check whether the script runs properly, by running
## 'cacheSolve(y)' again, the message 'getting cached data' is returned.


