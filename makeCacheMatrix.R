makeCacheMatrix <- function(A = matrix()) {
  B <- NULL
  set <- function(y) {
    A <<- y
    B <<- NULL
  }
  get <- function() A
  setInverse <- function(inverse) B <<- inverse
  getInverse <- function() B
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}
cacheSolve <- function(x, ...) {
  A <- x$getInverse()
  if(!is.null(A)) {
    message("getting cached data")
    return(A)
  }
  data <- x$get()
  A <- solve(data, ...)
  x$setInverse(A)
  A
}

cacheSolve<- function(x, ...) {
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



