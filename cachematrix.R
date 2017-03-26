## This is a pair of functions that cache the inverse of a matrix

## The first function creates a matrix that set the value of a matrix, 
## get the value of the matrix, set the value of the inverse of
## the matrix, get the value of the inverse of the matrix

makeCacheMatrix <- function(x) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function()
    x
  setinverse <- function(inverted) m <<- inverted
  getinverse <- function()
    m
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}


## the following funtion calculates the inverse of the special
## matrix created with the above function

cacheSolve <- function(x) {
  m <- x$getinverse()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}

direct <- matrix(c( 1, 4, 3, 2), 2, 2)
print(direct)
inverted <- cacheSolve(makeCacheMatrix(direct))
print(inverted)
print(direct %*% inverted)
