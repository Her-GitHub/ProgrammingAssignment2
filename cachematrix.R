## Write a pair of functions that cache the inverse of a matrix.

## The next function (makeCacheMatrix) creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) 
{
  inver <- NULL
  set <- function(y) 
  {
    x <<- y
    inver <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inver <<- inverse
  getinverse <- function() inver
  list(set = set, get = get, setinverse = setinverse,
       getinverse = getinverse)
}


## The next function (CacheSolve) calculates the inverse of the matrix 
## returned by function makeCacheMatrix

cacheSolve <- function(x, ...) 
{
## Return a matrix that is the inverse of 'x'
  inver <- x$getinverse()
  if (!is.null(inver)) 
  {
    message("Receiving Cached Data")
    return(inver)
  }
  data <- x$get()
  inver <- solve(data, ...)
  x$setinverse(inver)
  inver
}
