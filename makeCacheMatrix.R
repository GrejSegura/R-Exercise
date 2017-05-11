
set.seed(123)

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y = matrix()) {
    x <<- y*
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}


matrix <- matrix(c(runif(4, 5, 20), runif(4, 5, 20), runif(4, 5, 20), runif(4, 5, 20)), nrow = 4, ncol = 4, byrow = TRUE)
matrix <- as.matrix(matrix)
