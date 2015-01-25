## MakeCacheMatrix 
## The function takes a matrix as input
## It works together with cacheSolve
## It first flush Cache
## As the matrix can be only be set through set function
## whenever the matrix is changed new inverse will be calculated


makeCacheMatrix <- function(x = matrix()) {
m <- NULL

set  <- function(y) {
  x <<- y
  m <<- NULL
  }
get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
## next line of code is last line of function therefore it is
## the result of makevector. It is a list of names and values
  list( set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Function cacheSolve calculates the inverse of a matrix
## It first get the getinverse value that is part of MakeCacheMatrix
## Output. If the value of getinverse is different from NULL then
## inverse has been already calculated and get the value from the Cache
## Otherwise calculate the inverse of the matrix and passes the value
## to setinverse value to be cached.

cacheSolve <- function(x, ...) {
  ## Asign to m the value of the cached matrix
  m <- x$getinverse()
## if m is different from null then inverse has already been calculated
## it prints the message and return th value of m that is stored in parent environment
  if(!is.null(m)){
    message("getting cached inverse matrix")
    return(m)
  }
## if is null then inverse need to be calculated
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
