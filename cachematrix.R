## Based on the functions from the assignment examples, my following functions, when used in tandem, output the inverse of a matrix using a caching system (the first function contains a cache for the matrix, which the second function calls upon in order to solve). 

## The makeCacheMatrix function starts by creating an empty object "inv", and by setting objects 'x" and "inv". 
## Thereafter, "get" is an anonymous function that returns the matrix that was assigned the value of "x" by the user. 
## Using setInverse we replace the object "inv" with a new one when relevant (in the cacheSolve function).
## Finally, the getInverse function calls to output object "inv", which will become our matrix's inverse in the next function. 


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL 
  set <- function(y) { 
    x <<- y
    inv <<- NULL
  }
  get <- function() x 
  setInverse <- function(inputInverse) {
    inv <<- inputInverse 
  } 
  getInverse <- function() inv 
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## the CacheSolve function is comprised of two arguments: x, which is the cache to which we refer back (in function makeCacheMatrix), and the matrix, which itself is called x in the example I provided.
## First, NULL is assigned to object "inv" from the cache getInverse, and the if statement is skipped because its criterion is not met. 
## The original matrix is retrieved from function get, and assigned to object "data".
## The inverse of the original matrix is produced with the solve function, and assinged to object "inv". 
## The function in setInverse is used to replace the "inv" in the cache with the new "inv" which contains the inverse matrix. The result is printed. 

cacheSolve <- function(x, matrix) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(matrix)
  x$setInverse(inv) 
  print(inv)
}

x <- matrix(1:4, nrow = 2, ncol = 2) ##example matrix
cache <- makeCacheMatrix(x)
cacheSolve(cache, x)
