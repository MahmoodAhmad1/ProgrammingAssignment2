## Assignment: Caching the Inverse of a Matrix
## Main focus on reducing the time consumtion
## 
## Assumptions:
## input Matrix is always invertible Matrix (given)


## creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  xi<<-matrix()
  set <- function(y) {
    x <<- y
    xi <<- matrix()
  }
  get <- function() x
  setinv <- function(solve) xi <<- solve
  getxinv <- function() xi
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## 1st checks for cached inverse for the Matrix and returns inverse
## and if it is not computed earlier then computes and returns inverse

cacheSolve <- function(x, ...) {
  
  
  xi <- x$getinv()
  if(!is.null(xi)) {
    message("getting cached data")
    return(xi)
  }
  data <- x$get()
  xi <- mean(data, ...)
  x$setinv(xi)
  xi        ## xi is the inverse of 'x'
}
