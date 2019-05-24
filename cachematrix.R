## The following pair of functions cache the value of the inverse of a matrix.
## Computing the inverse is a computationally expensive process.
## Hence,caching or temporary storage helps in saving time and memory.

## The following function creates the matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## The function calculates the inverse of the matrix object created by 
## makeCacheMatrix. If the value has already been calculated and the 
## matrix doesnt change, it returns the value from the cache.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}
