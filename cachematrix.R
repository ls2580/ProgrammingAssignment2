## to avoid costly calculation the below functions 
## are able caching the inverse of a matrix rather than computing it every time
## please see a test case in the end of this file

## The makeCacheMatrix function creates a list, that 
## contains a function to:

##1)set the value of the matrix
##2)get the value of the matrix
##3)set the value of the inverted matrix to setinvrs
##4)get the value of the inverted matrix to getinvrs

makeCacheMatrix <- function(x = matrix()) {

    invrs <- NULL
    set <- function(y) {
        x <<- y
        invrs <<- NULL
    }
    get <- function() x
    setinvrs <- function(inv) invrs <<- inv
    getinvrs <- function() invrs
    list(set=set, get=get, setinvrs=setinvrs, getinvrs=getinvrs)

}

## the below function checks if inverted matrix was stored by the makeCacheMatrix function
## in the x$getinvrs object. if the object is NULL the function inverts the matrix stored in 
## the x$get object and sets it to the x$getinvrs object.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

  c <- x$getinvrs()
  if (!is.null(c)) {
          message("getting cached data!")
          return(c)
  }
  data <- x$get()
  c <- solve(data, ...)
  x$setinvrs (c)
  c

}


## Test:
## -> z <- matrix(1:4,2,2)

## -> z

##      [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## -> q <- makeCacheMatrix(z)

## -> q$get()

##      [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## > q$getinvrs()
## NULL

## the first execution checks if the c(value in cache) is not empty. if it is NULL 
## invertes the matrix using solve function, returns the inverted matrix, sets $setinvrs,
## so the message("getting cached data!") is not populated
 
## -> cacheSolve(q)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

## the second run populates inverted matrix from cache
## -> cacheSolve(q)
## getting cached data!
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

