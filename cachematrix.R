## Put comments here that give an overall description of what your
## functions do
# These functions create the inverse of a matrix 
# and cache that inverse, so that it can be recalled
# if it exists, without needing to recompute it.
# This might be worthwhile to save processing time.


## Write a short comment describing this function
# This function stores a given matrix and allows 
# you to check to see if the inverse is created yet.
#
# 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
# This function checks to see if an inverse of the matrix
# has been created yet. If not, it creates the inverse
# using the R solve() function. If it already exists in the
# cache, it retrieves the inverse rather than creating it again.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m

}
#     to test:
# my_matrix <- matrix(c(2,0,0,2),2,2)
#     now create try makeCacheMatrix. It will give you "NULL" results
#     since the inverse doesn't exist yet
# my_cached_matrix <- makeCacheMatrix(my_matrix)
# my_cached_matrix$get()
    
# my_cached_matrix$getinverse()
#   this is null, as the inverse hasn't been created yet

#   now use the second function to actually create the inverse
# cacheSolve(my_cached_matrix)
#   this creates and then returns the inverse of the matrix provided
#   now run that again
# cacheSolve(my_cached_matrix)
#   now you get the message that the function is retrieving 
#   the cached version since it exists
#   Therefore, the function did not create the inverse again, 
#   potentially saving processing time.
#   Directly access the inverse by again calling my_cached_matrix$getinverse()
# my_cached_matrix$getinverse()
#   Now the inverse is displayed, rather than NULL as before
