## Put comments here that give an overall description of what your
## functions do

## The first function, `makeCacheMatrix` creates a special "vector", which 
## is a list containing a function that 
## will set the value of the matrix, get the value of the matrix, set the 
## value of the inverse, and get the value of the inverse. 


makeCacheMatrix <- function(x = matrix()) {
  matrx <- NULL
  set <- function(y) {
    x <<- y
    matrx <<- NULL
  }
  get <- function() x
  setmatrx <- function(solve) matrx <<- solve
  getmatrx <- function() matrx
  list(set = set, get = get,
       setmatrx = setmatrx,
       getmatrx = getmatrx)
}


## The following function calculates the inverse of the special "vector"
## created above. It first checks to see if the inverse has already been 
## calculated. If so, it gets the inverse from the cache and skips the 
## computation. Otherwise, it calculates the inverse of
## the data and sets the value of the inverse in the cache via the `setmatrx`
## function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  matrx <- x$getmatrx()
  if(!is.null(matrx)) {
    message("getting cached data")
    return(matrx)
  }
  data <- x$get()
  matrx <- solve(data, ...)
  x$setmatrx(matrx)
  matrx
}
