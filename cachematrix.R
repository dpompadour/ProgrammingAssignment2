## makeCacheMatrix -- Function that creates vector  
## with following list of functions:
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse
## 4.  get the value of the inverse
## Takes numeric matrix as input parameter

makeCacheMatrix <- function(x = numeric()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## Calculates inverse of matrix and stores it to 
## makeCacheMatrix. Pulls inverse from makeCacheMatrix
## if already calculated. Takes makeCackematrix
## vector as input. Throws error if input matrix
## is not invertible.

cacheSolve <- function(x, ...) {
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}
