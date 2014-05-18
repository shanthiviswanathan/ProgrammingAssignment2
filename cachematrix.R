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
    ## Set Matrix
    set <- function(y) {
      x <<- y
      inversion <<- NULL
    }
    
    #Get Matrix
    get <- function() x
    
    #Set cached matrix
    setCache <- function(inverted_matrix) inversion <<- inverted_matrix
    
    #Get cached matrix
    getCache <- function() inversion
    
    #Return list of functions
    list(set = set, get = get,
         setCache = setCache,
         getCache = getCache)
    
}


## Write a short comment describing this function
# the function checks  if the inversion matrix exists or not with the function call getCache()
#
# if the call returns a not null value then the inverted matrix exists in cache and hence that value is returned.
#
# Finally, the inverted matrix is cached in the makeCacheMatrix list using the function setCache(inversion)

cacheSolve <- function(x, ...) {
  ## Check if the inversion matrix exists
  inversion <- x$getCache()
  if(!is.null(inversion)) {
    message("Retrieving from cache")
    return(inversion)
  }
  ## Not in cache and hence calculate inversion using solve.
  data <- x$get()
  inversion <- solve(data, ...)
  ##Set Inversion Matrix in cache
  x$setCache(inversion)
  inversion  
}
