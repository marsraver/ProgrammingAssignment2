## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix creates object "CacheMatrix" which is really a list 
## containing a function to set/get matrix and set/get reversed matrix

makeCacheMatrix <- function(x = matrix()) {
  ## check the x class
  if ( class(x) != "matrix" ) 
    stop( "x should be a matrix" )
  
  ## check if matrix is square
  if ( nrow(x) != ncol(x) )
    stop( "matrix x is not square" )
  
  ## prepare the reverse matrix with a name "reverse"
  reverse <- NULL
  
  ## set the value of the matrix
  set <- function(y) {
    x <<- y
    
    ## clean the cached reverse if exists
    reverse <<- NULL
  }
  
  ## get the value of the matrix
  get <- function() x
  
  ## set the value of the reverse
  setreverse <- function(reversed) reverse <<- reversed
  
  ## get the value of the reverse
  getreverse <- function() reverse
  
  ## return the list object
  list(set = set, get = get,
       setreverse = setreverse,
       getreverse = getreverse)
}


## Write a short comment describing this function
## cacheSolve calculates the reverse of the special "CacheMatrix" created with the first function. 
## It first checks to see if the reversed matrix has already been calculated. 
## If so, it gets the matrix from the cache and skips the computation. 
## Otherwise, it calculates the reverse matrix by solve() function, caches the result and returns it

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  reverse <- x$getreverse()
  
  ## try if reversed matrix was cached
  if( !is.null(reverse) ) {
    message("getting cached data")
    return(reverse)
  }
  
  ## calculate reversed matrix, error appears if 
  data <- x$get()
  reverse <- solve(data, ...)
  
  ## save reversed matrix
  x$setreverse(reverse)
  
  ## return result
  reverse
}
