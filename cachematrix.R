## The program makes it possible to calculate the inverse of a matrix and cache it for future use

## ----------------------- makeCacheMatrix ----------------------------
## Create a special object that can cache a matrix and its inverse
## Input: a matrix
## Output: An object with functions get, set, getinv and setinv
##         get - returns the stored matrix
##         set - allows changing the matrix stored
##         getinv - gets the inverse of the matrix. 
##         setinv - sets the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
   inv <- NULL                        ## Inverse of x
   set <- function (y) {              ## Set the value
     x <<-y
     inv <<- NULL
   }
   
   get <- function()                   ## Get the value
   {
     x
   }
   
   setinv <- function(inverse)        ## Set the inverse
   {
     inv <<- inverse
   }
   
   getinv <- function()               ## Get the inverse
   {
     inv
   }
   
   list (set=set, get=get, setinv=setinv, getinv=getinv)
   
}


## ----------------------- cacheSolve ------------------------
## Return a matrix that is the inverse of 'x'
## Input: A list object created by calling makeCacheMatrix
## Output: Inverse of the matrix stored in the list object. 
##         The first call computes the matrix and caches it for subsequent calls.

cacheSolve <- function(x, ...) {      
  ## Look for inverse in the cache
  inv <- x$getinv()           
  if (!is.null(inv)) {
    ## found it, return it
    message ("getting cached data")
    return (inv)
  }
  ## else compute the inverse
  data <- x$get()
  inv <- solve(data)
  ## cache it for use next time
  x$setinv(inv)
  ## return
  inv
}
