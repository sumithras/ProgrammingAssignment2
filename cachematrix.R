## Create a mechanism to "remember" the inverse of a matrix, to avoid executing a costly operation multiple times


## Create a special "matrix", which is really a list containing 4 elements that are functions to
##    1. set the value of the matrix
##    2. get the value of the matrix
##    3. set the value of the inverse
##    4. get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Calculate the inverse of the special "matrix" created with the above function.
## It first checks to see if the inverse has already been calculated. If so, it gets
## the inverse from the cache and skips the computation. Otherwise, it calculates 
## the inverse and saves it in the cache via the setinv function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

