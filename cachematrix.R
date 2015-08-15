## The functions makeCacheMatrix and cacheSolve allow a user to cache the value 
## of a matrix along with its inverse and quickly retrieve the inverse without 
## re-calculating if it has already been cached. If it has not been cached then
## the function cacheSolve will calculate the inverse and store it in cache for
## faster retrieval on the next function call.

## This function is run on a matrix and returns a list of functions that: 
## 1.) cache the value of the matrix via lexical scoping 2.) allow a user to re-
## trieve the value of the matrix 3.) cache the value of the matrix inverse via
## lexical scoping and 4.) allow a user to retrieve the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
      
      #pre-allocate matrix inverse to null
      inv <- NULL
      
      #function which sets value of x in the makeCacheMatrix's function closure
      #to the value passed into makeCacheMatrix
      set <- function(y){
            x <<- y
            inv <<- NULL
      }
      
      #function to return x
      get <- function() x
      
      #function that will set variable "inv" in makeCacheMatrix's closure to its
      #input
      set.inv <- function(solve) inv <<- solve
      
      #returns "inv"
      get.inv <- function() inv
      
      #return list of functions to be passed to cacheSolve
      list(set = set, get = get, set.inv = set.inv, get.inv = get.inv)
}


## This function is run on the list of functions returned by makeCacheMatrix. It
## first attempts to get the cached inverse and if it has been set it will print
## a message and return the cached inverse. If it has not been set it will
## return the original cached matrix, calculate its inverse, then cache the 
## inverse using the set.inv function from the list object.

cacheSolve <- function(x,...) {
      
      #run list of functions "get.inv" and if it doesn't return a null value,
      #print a message and return its value from cacheSolve
      inv <- x$get.inv()
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      
      #if inv is NULL, then retrieve the original matrix, calculate its inverse,
      #and set its inverse into the function closure of cacheSolve's input list
      data <- x$get()
      inv <- solve(data,...)
      x$set.inv(inv)
      
      #return the matrix inverse
      inv
}