## makeCacheMatrix 1) stores a supplied matrix object which is associated with the function
##                 2) creates a list of functions associated with the function that can be called with that matrix object
##                 3) sets the initial state of a variable m to NULL and save in this function's environment when storing the supplied matrix
##                 A) correct function names assigned when compared to examples provided

makeCacheMatrix <- function(x = matrix()) { ## matrix object passed for storage and computation of inverse of 'x' if not already computed
  
  m <- NULL                                 ## clear the cached value m when calling makeCachematrix with presumably new values for a new matrix
  
  get <- function() {                       ## get the value of the current matrix requested, namely 'x'
    x
  }
  setmatrixinverse <- function(setmatrixinverse) { ## store the calculated inverted matrix value result in cache variable m 
    m <<- setmatrixinverse                         ## inside the called from makeCacheMatrix function's envrionment
  }
  getmatrixinverse <- function() {                 ## return the already computed and saved inverted matrix value
    m
  }
  list(get = get, setmatrixinverse = setmatrixinverse, getmatrixinverse = getmatrixinverse)  ## list of created functions to be called
}

## cacheSolve 1) Return a matrix that is the inverse of 'x' using stored result from cache if already calculated
##            2) If not already calculate for current value of 'x', then calculate inverse of matrix 'x'and store the result

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getmatrixinverse()  ## get current inverse matrix value associated with matrix 'x' from makeCacheMatrix
  if(!is.null(m)) {          ## if already computed, then return this value
    message("getting cached inverse data for x")
    return(m)
  }                          
  ## otherwise compute and store the inverse matrix value for matrix 'x'
  data <- x$get()
  m <- solve(data, ...)      ## compute the inverse for matix 'x'  
  x$setmatrixinverse(m)      ## store the inverse matrix result from matrix 'x' in cacche variable m in function
  m                          
}