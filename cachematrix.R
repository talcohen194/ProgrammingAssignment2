## i am going to wirte 2 functions that will work together.
##their job is to cache the inverse of a matrix


## The "makeCacheMatrix" function creates an R object that stores
##a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  ##initializing key object
  s <- NULL
  
  ##setting the data values within the object
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  
  ##retrieving data within the object
  get <- function() x
  
  ##defining a setter and a getter
  setSolve <- function(solve) s <<- solve
  getSolve <- function() s
  
  ##assigning all of the functions to a "list"
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
  
}

## the "cacheSolve" function takes an argument from the output of "makeCacheMatrix"
## function and retrieves the inverse cached value that is in the "makeCacheMatrix" 
## environment, or calculate it

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ##assigning s to retrieve the inverse of the object
  s <- x$getSolve()
  
  ##assigning a cach massage and a return value if we calculate s in the past
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  
  ##if s is null that means that the "makeCacheMatrix" function didnt calculate the inverse
  ##of the matrix in the past so we need to calculate it now
  data <- x$get()
  s <- solve(data, ...)
  x$setSolve(s)
  s
}

