## creates a special "matrix" containing a function to

##set the value of the matrix
##get the value of the martrix
##set the value of the inverse matrix
##get the value of the inverse matrix


## creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <-NULL
  set <- function(y=matrix()) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinver <- function(x) inv <<- solve(x)
  getinver <- function() inv
  list(set = set, get = get,
       setinver = setinver,
       getinver = getinver)
}


## computes the inverse of the special "matrix" returned by makeCacheMatrix. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinver()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinver(inv)
  inv
}
