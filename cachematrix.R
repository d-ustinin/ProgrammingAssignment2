## Here are two functions - one constructs 'matrix with cached inverse' object,
## another computes inverse matrix for it

## creates 'matrix with cached inverse' object which can cache its inverse matrix to
## inv_matrix variable
## 

makeCacheMatrix <- function(x = matrix()) {
  inv_matrix <- NULL ## cached inverse matrix
  set <- function(y) { ## set function - resets inverse matrix when original matrix is changed
    x <<- y
    inv_matrix <<- NULL
  }
  get <- function() x
  set_inverse <- function(inv_mat) inv_matrix <<- inv_mat
  get_inverse <- function() inv_matrix
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse) ## returns list of functions to set, get matrix and its inverse values

}


## function which returns cached inverse matrix if it's already calculated 
## and calls 'solve' function if inverse matrix is not calculated yet
## if 'solve' is called then it's result is returned and stored in cash

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv_matrix <- x$get_inverse()
  if(!is.null(inv_matrix)) {
    message("getting cached inverse matrix")
    return(inv_matrix)
  }
  data <- x$get()
  inv_matrix <- solve(data, ...)
  x$set_inverse(inv_matrix)
  inv_matrix
}
