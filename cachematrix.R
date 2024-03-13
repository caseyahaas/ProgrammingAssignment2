## This is a set of functions that saves the time needed to find the inverse of a matrix. By cacheing the inverse, that computation only needs to be done once.
## So every time you need to pull the inverse, this function will first check the magic drawer of already done work and see if that's really necessary.
## If some time can be saved by pulling it out of the drawer, it will return that inverse for you. If it's not there yet, it will calculate it and put 
## a copy in the drawer JIC you need it later.

## The following function creates a special "matrix" object that can cache its inverse. It does this with the following functions:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The following function calculates the inverse of the special "matrix" created with the above function. It checks to see if the inverse has
## already been calculated. If so, it gets the inverse from the cache and skips the computation. If not, it calculates the inverse of the matrix 
## and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)){
          message("getting cached data")
          return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
