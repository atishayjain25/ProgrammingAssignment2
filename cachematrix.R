## This code explains the Object Oriented approach in R.
## makeCacheMatrix creates an R object which can cache the inverse of its matrix variable in its own environment.
## cacheSolve takes an object of this type and returns a cached inverse of the matrix passed to it if present.


## This function takes a matrix as an input and creates a list object having 4 elements, each of them a function for
## get, set, getinverse and setinverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function takes a list object of the above mentioned type. If a cached inverse matrix is already available then it
## returns it. Else, it computes the inverse, cache it and returns it.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
        ## Return a matrix that is the inverse of 'x'
}

## Test function to test the matrix caching code
testCacheMatrixInverse <- function() {
  a <- matrix(c(4,0,0,1,0,0,1,0,0,2,2,0,0,0,0,1), nrow = 4, ncol = 4);
  b = makeCacheMatrix(a);
  ainv <- cacheSolve(b);
  ainv <- cacheSolve(b);
  identityExpected <- b$get()%*%ainv;
  identityActual <- matrix(c(1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1), nrow = 4, ncol = 4);
  areequal <- dim(identityActual) == dim(identityExpected) && all(identityActual == identityExpected);
  areequal;
}
