##  'makeMatrix' function will take a matrix, storing it, 
##  allowing recall, using 'get'
##  The 'cacheSolve' function can be used to check and return inverse of matrix
##  from previous function
##  Inverse of the matrix is set internally utilising 'setinverse'
##  Inverse of the matrix can be retrived using 'getinverse'

##  This stores matrix, and allows sotring and recall of matrix & inverse

makeMatrix <- function(x = matrix()) {
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


##  This finction checks for inverse of matrix, 
##  recalling and/or creating if not present

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
    ## Return a matrix that is the inverse of 'x'
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}