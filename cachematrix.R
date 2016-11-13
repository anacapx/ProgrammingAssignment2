## The two functions will receive a matrix, calculate its inverse
## and store it in cache.


makeCacheMatrix <- function(x = matrix()) { 
      ##makeCacheMatrix: Stores de matrix assigned in arguments in 'x' and
      ##Defines 4 functions: set, get, setinv, getinv

  m <- NULL             ## 'm' is initialized an set to NULL ('m' will receive inverse matrix)
  
  set <- function(y) {  ## SETTER: Defines set function of 'x'
    x <<- y             ## '<<-' assigns the input argument to 'x' and 'm' objects in parent environment  
    m <<- NULL          ## when 'x' changes with set function, the cached value of 'm' is cleared
  }
  
  get <- function() x                 ## GETTER: Defines get fuction of 'x'
  
  setinv <- function(arg1) m <<- arg1 ##SETTER for inverse matrix
  
  getinv <- function() m              ## GETTER for inverse matrix
  
  list(set = set, get = get,          ## Creates new object (list), with the named functions.
       setinv = setinv,               ## naming the function allow us to use '$'
       getinv = getinv)
  
}


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinv()              
  if(!is.null(m)) {                     ## Tests if 'm' is null
    message("getting cached data")      ## If 'm' is not null, 'm' value is retrieved from cache
    return(m)
  }
  data <- x$get()                      ## object 'data' receives the value of 'x'
  
  m <- solve(data, , ...)              ## Calculates the inverse matrix 'x' and
       x$setinv(m)                     ## 'm' receives the inverse matrix
  
  m                                    ## 'm' is printed
  
  }



