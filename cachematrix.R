## Caching the Inverse of a Matrix
## eg: 
## my_matrix <- c(1:20)
## dim(my_matrix) <- c(4,5)
## x <- makeCacheMatrix(my_matrix)
## cacheSolve(x)


##This function takes a matrix as input variable
##and creates a list of functions:
##set: sets the matrix with "y", and inv with NULL
##get: caches the input matrix x
##setinverse: caches the inverse when called upon, initially it is empty
##getinverse: returns the cached inverse, initially it is NULL

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


##This function assumes the input matrix is already cached
##i.e. the input matrix has already been used by makeCacheMatrix()
##for the first time, inv which is the inverse will be na
##in this case, func get() data, calc the inverse, and setinverse()
##for return visits, func getinverse() will work

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if (!is.null(inv)) {
    print ("retrieving cached inverse")
    return (inv)
  }
  else {
    data = x$get()
    inv <- solve(data,...)
    x$setinverse(inv)
    return (inv)
  }
}
