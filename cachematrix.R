## These two functions create a named list that contains
## a matrix, the inverse of the matrix and functions
## to get and set each of the matrix and it's inverse, 
## taking advantage of caching the inverse

## this functions takes a matrix
## and returns the named list described above

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inver) inv <<- inver
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function takes a named list as its argument, 
## ideally one created from the function above. 
## This function retrieves the inverse from the 
## cache if it is available, otherwise it computes
## and caches it and returns the inverse

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)){
    message("getting cashed data")
    return(i)
  }
  
  data <- x$get()
  i <- solve(data)
  x$setinv(i)
  i
}
