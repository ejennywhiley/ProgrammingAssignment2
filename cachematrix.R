## These are a pair of functions that either calculate the inverse of a matrix or return a cached value in the case the inverse was already calculated

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
     m <- NULL ##m is a free variable
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     get <- function() x
     setinv <- function(solve) m <<- solve
     getinv <- function() m
     list(set = set, get = get,
          setinv = setinv,
          getinv = getinv)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x = matrix(), ...) {
     ##check if inverse has already been calculated and cached, and if so return it
     m <- x$getinv()
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     matrix <- x$get()
     m <- solve(matrix) ##calc inverse of matrix
     x$setinv(m) ##store newly calculated inverse
     m ##output inverse
}
