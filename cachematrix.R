## create a function that can store and retreive the inverse of a matrix (the cache) and another function that computes the inverse of a matrix or can access this cache created in the first function to retreive the inverse it it hasnt been computed yet

## creates a cache (a list) to store the values of a matrx and it's inverse as well as the functions to retreive these values

makeCacheMatrix <- function(X = matrix()) {
  inv <- NULL
  set <- function(Y) {
    X <<- Y
    inv <<- NULL
  }
  get <- function() X
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## accesses the cache created in the first function to see if the inverse has already been computed and retreives this inverse, if not, then it computes the inverse

cacheSolve <- function(X, ...) {
  inv <- X$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- X$get()
  inv <- solve(data, ...)
  X$setinverse(inv)
  inv
}