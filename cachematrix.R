## This function returns inverse matrix from a cached object in R
## It also checks if the input matrix has not changed during the computation.

# The makeCacheMatrix function creates a list containing a function to
# 1. set the matrix
# 2. get the matrix
# 3. set the inverse of the matrix
# 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
      matrixinv<-NULL
      set<-function(y){
          x<<-y
          matrixinv<<-NULL
      }
      get<-function()x
      setinverse<- function(inverse) matrixinv <<-inverse
      getinverse<- function() matrixinv
      list(set=set, get=get, 
           setinverse=setinverse, 
           getinverse=getinverse)
}

  

## This function returns the inverse of the matrix. It checks if the inverse has already been computed.
# If yes, gets the results. If no, computes the invese and sets the value in the cache.

cacheSolve <- function(x, ...) {
  matrixinv<- x$getinverse()
  if(!is.null(matrixinv)) {
    message("getting cached data...")
    return(matrixinv)
  }
  data <- x$get()
  matrixinv<- solve(data, ...)
  x$setinverse(matrixinv)
  matrixinv
}