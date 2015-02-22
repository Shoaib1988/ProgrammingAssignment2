## MakeCacheMatrix function takes a matrix as an argument, stores this
## matrix in a variable named x, caches its inverse in a variable named
##"inverse" and also keeps track of whether the matrix has been changed
## in a boolean variable named "changed". It returns a list consisting of
## five functions: one for accessing the stored matrix, one for accessing
## the value of inverse stored in cache, one for accessing the variable
## "changed" to find out whether the matrix has been changed, one for 
## changing the matrix which also automatically sets the value of the
## variable "changed" to "TRUE", and one for assigning a calculated inverse
##value to the cache variable "inverse".

makeCacheMatrix <- function(x = matrix()) {
  changed <- FALSE
  inverse <- NULL
  
  change_matrix <- function(y){
    x <<- y
    changed <<- TRUE
  }
  
  get_changed <- function() changed
  get_matrix <- function() x
  set_inverse <- function(i) inverse <<- i
  get_inverse <- function() inverse
  
  list(change_matrix=change_matrix, get_changed=get_changed,
       get_matrix=get_matrix, set_inverse=set_inverse,
       get_inverse=get_inverse)
}


## The cacheSolve function takes the value of the matrix stored in the 
## makeCacheMatrix written above as an argument. It then determines whether
## the matrix was changed or not and whether its inverse is already stored
## in cache. It does this by invoking the get_changed() and get_inverse()
## functions which are part of the list returned by makeCacheMatrix. If the
## inverse is already stored in cache and the matrix has not been changed,
## it prints a message indicating that it has used the cached value and
## returns this cached value. Otherwise, it accesses the matrix stored in
## makeCacheMatrix function, calculates its inverse, returns this calculated
## value and stores this calculated value to the cache using set_inverse()
## function which is part of the list returned by makeCacheMatrix.

cacheSolve <- function(x, ...) {
  changed <- x$get_changed()
  inverse <- x$get_inverse()
  
  if ((!is.null(inverse)) && (changed=="FALSE")){
    
    print("Matrix has not been changed and inverse matrix already in cache.")
    print("So using the value stored in cache.")
    return(inverse)
  }else{
    
    matrix <- x$get_matrix()
    inverse <- solve(matrix, ...)
    x$set_inverse(inverse)
    inverse
    
  }
  ## Return a matrix that is the inverse of 'x'
}
