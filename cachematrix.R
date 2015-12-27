## Functions to compute the inverse of a matrix
## if it was already done and saved in the cache

## This function creates a matrix-object that can cache its own inverse

makeCacheMatrix <- function(x = matrix()) {
      invx <- NULL
      set <- function(y){
            x <<- y
            invx <<- NULL
      }
      get <- function() x
      setinv <- function(solve) invx <<- solve
      getinv <- function() invx
      list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## This function computes the inverse of a matrix
## but also checks if the inverse has already been computed and saved in the cache

cacheSolve <- function(x, ...) {
      invx <- x$getinv()
      if(!is.null(invx)) {
            message("getting cached data")
            return(invx)
      }
      tocomp <- x$get()
      invx <- solve(tocomp,...)
      x$setinv(invx)
      invx
}