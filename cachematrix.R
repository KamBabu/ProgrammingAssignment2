## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## The below code has  a pair of functions that cache the inverse 
## of a matrix.

## Function makeCasheMatrix creates a special matrix and caches
## the value in an environment away from the calling function's work environment (Lexical 
## Scoping). The output of the function is cached and is been recomputed by second function..

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y)
  {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## The following function calculates the Inverse of the special matrix created
## from the above function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
   m <- x$getInverse()
   if (!is.null(m)) {
     message("getting cashed data")
     return(m)
   }
   data <- x$get()
   m <- inverse(data,...)
   x$setmean(m)
   m
}
