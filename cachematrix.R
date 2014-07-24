# makeCacheMatrix creates a special matrix containing several functions
# 1. set value of the matrix: set()
# 2. get value of the matrix: get()
# 3. set value of the inverse: setinverse()
# 4. get value of the inverse: getinverse()

# an example of how to check if the functions operate correctly:
# x <- makeCacheMatrix(rbind(c(3, 5, 6), c(10, 12, 30), c(45, 2, 1)))
# x$get()
# cacheSolve(x)
# x$getinverse()

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


# cacheSolve first checks if the matrix inversion has already been calculated. 
# If not, calculates inverse and sets the value.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
