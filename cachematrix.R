## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than computing it repeatedly
## Here are a pair of functions that cache the inverse of a matrix

## `makeCacheMatrix`: This function creates a square "matrix" object that can 
## cache its inverse, which is really a list containing a function to

## 1.  set the value of the square Matrix 
## 2.  get the value of the square Matrix
## 3.  set the value of the inverse of this square matrix
## 4.  get the value of the inverse of this square matrix

makeCacheMatrix <- function(x = matrix()) {
  invrs <- NULL
  set <- function(y) { #set the value of the square Matrix 
    x <<- y
    invrs <<- NULL
  }
  get <- function() x #get the value of the square Matrix
  
  setinverse <- function(solve) invrs <<- solve #set the value of the inverse of this square matrix
  getinverse <- function() invrs #get the value of the inverse of this square matrix
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## `cacheSolve`: This function computes the inverse of the square
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  invrs <- x$getinverse()
  if(!is.null(invrs)) {
    message("getting cached data")
    return(invrs)
  }
  data <- x$get()
  invrs <- solve(data, ...)
  x$setinverse(invrs)
  invrs  ## Return a matrix that is the inverse of 'x'
}
