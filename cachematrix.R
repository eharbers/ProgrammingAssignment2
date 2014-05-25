## Put comments here that give an overall description of what your
## functions do

## create a special special matrix as a list containing a function

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  ## set the value of the matrix
  set <-function(y) {
    x<<- y
    i<<-NULL
  }
  ## get the value of the matrix
  get <- function() x
  ## set the inverse of the matrix
  setinverse <- function(solve) i <<-solve
  ## get the inverse of the matrix
  getinverse <- function() i
  ## put it all in a list to make calling the function(s) easier
  list(set=set, get=get,
     setinverse = setinverse,
     getinverse = getinverse)
}


## calulate the invers (using solve) of the special vector (above)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i<-x$getinverse()
        ## Check if mean already exists
  if(!is.null(i)) {
      message("getting cached data")
      return(i)
  }
  data<- x$get()
  ## calculate the inverse
  i<-solve(data, ...)
  ## set the inverse in cache
  x$setinverse(i)
  i
}

## TEST WITH
## amatrix=makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
## amatrix$get()
## cacheSolve(amatrix)
## to see the message call cacheSolve(amatrix) again
