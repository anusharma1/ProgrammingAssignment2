## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix function will enables caching a matrix for matrix 
##inverse and to set the inverse in to cache when it does not exist
## Listing the 4 methods to set, get the matrix and to setinverse, 
##getinverse of the matrix.

## Write a short comment describing this function

##makeCacheMatrix: This function creates a special "matrix" object 
##that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
##cacheSolve: This function computes the inverse of the special "matrix" 
##returned by makeCacheMatrix above. If the inverse has already been 
##calculated (and the matrix has not changed), then the cachesolve should 
##retrieve the inverse from the cache.

## cacheSolve function will returns the inverse of a matrix.  


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting the data from cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}




