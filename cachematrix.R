## Takes a matrix and inititalizes the functions set, get, setinverse, and getinverse and the names i and x to the makeCacheMatrix() environment. Then uses cacheSolve() function to check if the listed function getinverse is cached. If not, it assigns the listed function setinverse to the inverse of the passed matrix. 


## Write a short comment describing this function
## This function creates a list of functions (set, get, setinverse, and getinverse) and the objects i and x and stores them within the parent environment (i.e. makeCacheMatrix() environment). 
## The makeCacheMatrix is assigned to the global environment.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
     x <<- y
     i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
# This function takes a function of makeCacheMatrix (e.g. cacheSolve(makeCacheMatrix(matrix(c(2, 4, -3, -7), nrow = 2)))).
# It is essential that the matrix set in the makeCacheMatrix is invertible (i.e. the determinant is not 0).
# This function checks the parent environment (i.e. makeCacheMatrix()) for the inverse of x via getinverse(); if it cached, it returns the cached inverse.
# If it is not cached, it creates the inverse with the setinverse() function stored within makeCacheMatrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)){
   message("getting cache data")
   return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
  
}

