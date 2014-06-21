## Caching the Inverse of a Matrix by julio1925
## 
## This is an R-script of two functions that creates a special "matrix that
## can cache its inverse
##
## The first one is "makeCacheMatrix" that creates a "matrix", which contains a function to
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the cached matrix
## 4. Get the value of the cached matrix
## 
## The second function is "cacheSolve" that computes the inverse of the special "matrix" 
## returned by makeCacheMatrix.
##
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
##
## Computing the inverse of a square matrix can be done with the solve function in R. 
## For example, if X is a square invertible matrix, then solve(X) returns its inverse



## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <-NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <-function(solve) m <<- solve
  getmatrix <-function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}



## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x = matrix(), ...) {
  m <- x$getmatrix()
  if(!is.null(m)){
    message("retrieving cached data")
    return(m)
  }
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setmatrix(m)
  m
}