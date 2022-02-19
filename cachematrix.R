## This function creates a special "matrix" object that can cache its inverse

## First I define the argument with default mode of "matrix"
## then I initialize inv as NULL; will hold value of matrix inverse
## define the set function to assign new value of matrix in parent environment
##if there is a new matrix, reset inv to NULL
## then I define the get fucntion - returns value of the matrix argument
## the I assigns value of inv in parent environment

makeCacheMatrix <- function(x = matrix()) { 
  inv <- NULL                              
  set <- function(y) {                     
    x <<- y                             
    inv <<- NULL                        
  }
  get <- function() x                    
  
  setinverse <- function(inverse) inv <<- inverse  
  getinverse <- function() inv                     
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

