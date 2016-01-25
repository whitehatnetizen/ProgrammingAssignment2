## These two functions enable the user to create a matrix
## and cache its inverse.  This is particularly useful because
## inverting a matrix can be a particularly CPU intensive operation
## and Neo shouldn't have to plug himself in every time.
## 

## makeCacheMatrix:
## This function uses the same format as the assignment example 
## of caching a vector

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  
  # set the matrix variables 
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # Plugs Neo into the matrix
  get <- function() x
  
  # Set the inverse of the matrix (Neo does his superman thing)
  setinverse <- function(inverse) inv <<- inverse
  
  # Gets the inverse 
  getinverse <- function() inv
  
  # concatenates into a list
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)	
}

## cacheSolve:
## This function can be used instead of the standard solve() function
## it will retrieve the cached version of the inverse of the matrix rather 
## than re-applying the solve() function
##

cacheSolve <- function(x, ...) {
  # check to see whether the inverse has been computed yet 
  # (do we really need to go up to broadcast level?)
  inv <- x$getinverse()
  
  
  if(!is.null(inv)) {
    # if there's something already there, pull the plug and return to the real
    message("Getting cached matrix")
    return(inv)
  }
  
  # If not, hack the signal but watch out for agents
  data <- x$get()
  
  # Find the Keymaker... I mean the Inverse of the function
  inv <- solve(data, ...)
  
  # Cache the output (good work team!)
  x$setinverse(inv)
  
  # Send it back to the Real
  inv    
}

## example
# create matrix and cache inverse
#x <- matrix(1:4, 2, 2)
#y <- makeCacheMatrix(x)
#z <- cacheSolve(y)
#z

# get cached value
#z2 <- cacheSolve(y)
#print(z2)
