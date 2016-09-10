## These two functions respectively generate and operate on "makeCacheMatrix" type objects that contain a matrix, and allow methods for storing and retrieving the results of previous computions of the same matrix's inverse.

## This function creates an object that contains a matrix x, as well as 4 methods: set(), get(), setinv() and getinv(). 
## These methods allow for the cached matrix to be set and retrieved, as well as its inverse to be set and retrieved.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(invers) inv <<- invers
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function first retrieves the already cached value of the matrix inverse, if it already exists, using the getinv() method. It stores the variable in "current_inverse."
## If there is a value present, the function returns "getting cached data" and returns current_inverse.
## Otherwise, the rest of the function executes. The matrix is retrieved using the get() method and stored in this_matrix.
## Next, the solve() function is called on this_matrix, passing along any arguments that are stored in the "...", and the result are stored in "this_matrix_inverse."
## The value of this_matrix_inverse is stored in the makeCacheMatrix obect x by using the "setinv" method.
## Finally, this_matrix_inverse is returned.

cacheSolve <- function(x, ...) {
  current_inverse <- x$getinv()
  if(!is.null(current_inverse)) {
    message("getting cached data")
    return(current_inverse)
  }
  this_matrix <- x$get()
  this_matrix_inverse <- solve(this_matrix, ...)
  x$setinv(this_matrix_inverse)
  this_matrix_inverse
}
