## Prepare a matrix to feed into the function cacheSolve() function below.
## Creates a list containing a function to
## -set the matrix
## -get the  matrix
## -set the inverse of the matrix
## -get the inverse of the matrix
## -makeCacheMatrix <- function(x = matrix()){
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...){
    m <- x$getinv()
  ## Check if the matrix is already stored in the cached data.
  ## If yes, the inverse will be retrieved from cached data
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ##If matrix is not in cached data, then compute invrse and store in cache.
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  ##return inverse of matrix
  m
}
