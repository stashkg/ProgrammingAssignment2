## Performance optimized functions to get inverse of a matrix
## 

## Creates a list of functions to get/set a matrix and to get/set cached inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  setmatrix <- function(y) {
    x <<- y
    inv <<- NULL
  }
  getmatrix <- function() x
  setinv <- function(inverse) {
    inv <<- inverse
  }
  getinv <- function() inv
  list(setmatrix = setmatrix, getmatrix = getmatrix,
       setinv = setinv,
       getinv = getinv)
  
}


## Function to update and retrieve the cached inverse of a matrix 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$getmatrix()
  inv <- solve(data)
  x$setinv(inv)
  inv
}

