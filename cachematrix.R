## Assuming the matrix x is invertible, the below functions can be used in a loop
## in place of the solve function to cache and return the inverse matrix rather
## than calculating it afresh each time it is needed.

## Example:
## 1. Define x
## x<-matrix(c(2,2,3,2),nrow=2,ncol=2)
## 2. View x
## > x
##      [,1] [,2]
## [1,]    2    3
## [2,]    2    2
## 3. Use cacheSolve to return the inverse of the matrix x:
## cacheSolve(makeCacheMatrix(x))
##      [,1] [,2]
## [1,]   -1  1.5
## [2,]    1 -1.0
## 4. Check that the matrix returned is the inverse:
## > x%*% cacheSolve(makeCacheMatrix(x))
##      [,1] [,2]
## [1,]    1    0
## [2,]    0    1


## The function makeCacheMatrix  creates a special "vector", 
## which is really a list containing a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
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


## The following function "cacheSolve" calculates the inverse matrix of the special "vector" 
## created with the above function. 
## However, it first checks to see if the inverse matrix has already been calculated. 
## If so, it gets the inverse matrix from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache 
## via the setsolve function.

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

