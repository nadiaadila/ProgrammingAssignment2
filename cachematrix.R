## To caching the inverse of matrix
## This function creates a special "matrix" object that can cache its inverse.
## We use this because it is better that compute it repeatedly 

makeCacheMatrix <- function(x = matrix()) {
  invs <- NULL
  set <- function(y){
    x <<- y
    invs <<- NULL
  }
  get <- function() x
  
  setInverse <- function(solveMat)  invs <<- solveMat
  getInverse <- function() invs
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}

## To compute the inverse of matrix
## This functions is to compute the inverse of special matrix return by makeCacheMatrix.
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invs <- x$getInverse()
  
  if(!is.null(invs)){
    message("getting cached data")
    return(invs)
  }
  
  data <- x$get()
  invs <- solve(data, ...)
  x$setInverse(invs)
  invs    
}
