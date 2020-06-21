## This operation is made up of 2 functions, the first will 
## store the data (a matrix) in cache and the second will 
## calculate the inversion of the cached matrix

## The first function of the series -> it will set the 
## values that will be needed by the second function to 
## calculate the inversion

makeCacheMatrix <- function(x = matrix()) {
  inv_value <- NULL
  set <- function(set_x) {
    x <<- set_x
    inv_value <<- NULL
  }
  get <- function() x
  setmean <- function(inversion) inv_value <<- inversion
  getmean <- function() inv_value
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

## The cacheSolve function will take the cache values to 
## calculate the inversion of the matrix

cacheSolve <- function(x, ...) {
  inv_value <- x$getmean()
  if(!is.null(inv_value)) {
    message("getting cached data")
    return(inv_value)
  }
  data <- x$get()
  inv_value <- solve(data, ...)
  x$setmean(inv_value)
  inv_value
}
        ## Return a matrix that is the inverse of 'x'
