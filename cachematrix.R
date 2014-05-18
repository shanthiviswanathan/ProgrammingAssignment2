## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# The function takes a matrix x as an input
# it defines the following in its scope:
# variable "inversion" intead to hold the inverse
# four functions:
#   1- set
#   2- get
#   3- setinversion
#   4- getinversion
#
# The set function uses the <<- operator to assign the values defined at the scope of makeCacheMatrix environment level.
# The following three functions have x and inversion as free variables that aren't defined within their scopes, 
# so they search on the parent level - which is makeCacheMatrix level - in which they are defined. 
# accordingly they are able to set the variables using the <<- operator.
# 
# the get points to a function that returns the matrix
# the getinv points to a function that returns the inversion of the matrix.
# 
# the function returns a list that holds the four functions
makeCacheMatrix <- function(x = matrix()) {
    inversion <- NULL
    set <- function(y) {
      x <<- y
      inversion <<- NULL
    }
    get <- function() x
    setinversion <- function(inverted_matrix) inversion <<- inverted_matrix
    getinversion <- function() inversion
    list(set = set, get = get,
         setinversion = setinversion,
         getinversion = getinversion)
    
}


## Write a short comment describing this function
# the function checks  if the inversion matrix exists or not with the function call getinversion()
#
# if the call returns a not null value then the inverted matrix exists in cache and hence that value is returned.
#
# Finally, the inverted matrix is cached in the makeCacheMatrix list using the function setinversion(inversion)

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inversion <- x$getinversion()
  if(!is.null(inversion)) {
    message("getting cached data")
    return(inversion)
  }
  data <- x$get()
  inversion <- solve(data, ...)
  x$setinversion(inversion)
  inversion  
}
