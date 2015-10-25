## Implements mechanism for caching inverse of matrix by defining: 1) cached matrix 
## object obtained by calling makeCacheMatrix 2) Given a cached matrix object x, 
## function cacheSolve(x), returns the inverse by freshly computing, if it is not 
## already been computed.

## Example:
## A = cbind(c(1, 1/2), c(1/2, 1))
## Acache = makeCacheMatrix(A)
## cacheSolve(Acache)
##            [,1]       [,2]
## [1,]  1.3333333 -0.6666667
## [2,] -0.6666667  1.3333333
## cacheSolve(Acache)
## getting cached inverse
##            [,1]       [,2]
## [1,]  1.3333333 -0.6666667
## [2,] -0.6666667  1.3333333


## Write a short comment describing this function: The function implements
## getters/setters for data and inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## The function can be invoked on a cached matrix object provided by
## makeCacheMatrix object. The function does not repeat computation
## if it's already been computed.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached inverse")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv
}
