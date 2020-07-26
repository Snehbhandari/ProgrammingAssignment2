## Put comments here that give an overall description of what your
## functions do
## use the first function to input the matrix
## use the second function to get the cashed inverse matrix 

## Write a short comment describing this function
## This functions takes a matrix as an input then calculates the 
## inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  invr <- NULL
  set <- function(y){
    x <<- y
    invr <<- NULL
  }
  get <- function() x
  setinv <- function(solve) invr <<- solve 
  getinv <- function() invr
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
## This function checks for the chached inverse matrix and returns the 
## inverse of the matrix 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invr <- x$getinv()
  if(!is.null(invr)){
    message("getting cashed matrix inverse")
    return(invr)
  }
  data <- x$get()
  invr <- solve(data, ...)
  x$setinv(invr)
  invr
}
