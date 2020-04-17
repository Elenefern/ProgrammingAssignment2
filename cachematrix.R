## Functions that optimise the use of matrix inverses by caching its value the 
## first time is created.


## Function that receives a matrix and returns a list containing functions 
## to set/get the matrix and get/set its inverse. 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Function that receives the list of functions created by "makeCacheMatrix" and
## returns the inverse of the matrix by retrieving it from the cache or calculating
## it if it has never been calculated before.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)){
    # if already calculated return cached data
    message("getting cached data")
    return(inv)
  }
  # if not calculate it
  matrix <- x$get()
  inv <- solve(matrix,...)
  x$setinverse(inv)
  inv
}
