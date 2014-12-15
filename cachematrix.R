## The following two functions is makeCacheMatrix and cacheSolve 
## makeCacheMatrix function do the basic operation in cache 
## cacheSolve function do inverse matrix operation


## Keep the matrix and inverse matrix in cache
## input: matrix
## function: 
##  set(y)-> set the origin matrix to y. inverse matrix need to recompute, so set the inverse matrix to null 
##  get-> get the now origin matrix in cache
##  setinverse(inv_matrix)-> set the inverse matrix to inv_matrix in cache
##  getinverse-> get the now inverse matrix in cache

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse_matrix) inverse <<- inverse_matrix
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Get inverse cache matrix first. If having cache data then return the inverse matrix in cache, otherwise recompute the inverse matrix
## input: matrix X


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inverse <- x$getinverse()

  if(!is.null(inverse)) {
    message("getting cached inversed matrix")
    return(inverse)
  }
  matrix <- x$get()
  result <- try(solve(matrix))
  if (class(result)=="try-error"){
    print (geterrmessage())
  }
  x$setinverse(result)
}