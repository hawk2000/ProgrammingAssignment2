## This couple of functions work together in order to get the inverse
## of a matrix, caching the value of the inverse the first time that 
## is calculated to prevent a costly operation next time that the value
## is neeeded.

## Usage:

##  > o <- b <- rbind(c(3, 2, 1), c(4, 1, 0), c(0, 6, 5))
##  > o  
##       [,1] [,2] [,3]
##  [1,]    3    2    1
##  [2,]    4    1    0
##  [3,]    0    6    5

##  > om  <- makeCacheMatrix(o)

##  > cacheSolve(om)
##       [,1] [,2] [,3]
##  [1,]   -5    4    1
##  [2,]   20  -15   -4
##  [3,]  -24   18    5

##  > cacheSolve(om)
##  getting cached data
##       [,1] [,2] [,3]
##  [1,]   -5    4    1
##  [2,]   20  -15   -4
##  [3,]  -24   18    5


## Create from a matrix a special one with a list of functions and holder for
## cached inverse value

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inverse) i <<- inverse
  
  getinverse <- function() i
  
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Calculates the inverse of a special matrix created by makeCacheMatrix
## caching the result for future use

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m  
}