makeCacheMatrix <- function(A = matrix()) {
  #initialize the inverse matrix
  B <- NULL
  
  #set the input matrix and reset the inverse matrix
  set <- function(y) {
    A <<- y
    B <<- NULL
  }
  # get the original matrix
  get <- function() A
  #set the inverse matrix
  setInverse <- function(inverse) B <<- inverse
  
  #get the inverse matrix
  getInverse <- function() B
  
  #output
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
  #calculate the inverse matrix
  A <- solve(data, ...)
  
  #give the calculated value to setInverse function
  
  x$setInverse(A)
 
  #return the inverse matrix
  A
}




