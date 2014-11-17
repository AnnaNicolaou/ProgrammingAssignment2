## Put comments here that give an overall description of what your
## functions do
##########                Caching the Inverse of a Matrix
#The pair of functions compute and store the inverse of a matrix. If the inverse has already been 
#calculated (and the matrix hasn't changed), then the inverse is retrieved from the cache.


## Write a short comment describing this function

#makeCacheMatrix() takes a matrix as an argument and creates an "object" of type 'list'. 
#This object stores two things, the original matrix and what will be 
#the cached value (which is the inverse of the matrix), initially set to 'NULL'. 
#There are four functions, two to read ( 'get') the two things we are storing, and two functions to
#change ('set') them.  

######################################################################################################

makeCacheMatrix <- function(x = matrix()) { #input x will be a matrix
  
  s <- NULL       # s will be its inverse and it is reset to NULL every time that 
  # makeCacheMatrix is called
  
  set <- function(y){      # takes an input matrix
    x <<- y                # saves the input matrix
    s <<- NULL             # resets the inverse to NULL every time we have a new input matrix
  }
  
  get <- function()  x     # this function returns the value of the original matrix
  
  setinverse <- function(solve) s <<- solve   # this function is called by cacheSolve during the
  # first cacheSolve access and it will store the value 
  # using superassignment
  getinverse <- function() s                  # this will return the cached value to cacheSolve() on
  # subsequent accesses
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## Write a short comment describing this function

#cacheSolve() accesses the object created when makeCacheMatrix() is called.
#If the Inverse has not yet been calculated ('NULL') cacheSolve() 
#calculates it, stores it  in the object created by the call to makeCacheMatrix(),
#and then returns it.  If the Inverse has been  calculated earlier then cacheSolve() 
#simply fetches it and returns its value, saving the computing time required to
#calculate it again.  
##################################################################################

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  # the input x is the object created by makeCacheMatrix
  
  s <- x$getinverse()                # accesses the object 'x' and gets the value of the inverse
  if(!is.null(s)){                   # if inverse was already cached (not NULL) ...
    message("getting cached data")   # ... send this message to the console
    return(s)
  }
  data <- x$get()                   # we reach this code only if x$getinverse() returned NULL
  s <- solve(data, ...)             # then we have to calculate the inverse
  x$setinverse(s)                   # and store it
  s                                 # return the inverse to the code that called this function
}
