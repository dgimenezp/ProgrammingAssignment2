## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix
## Define workspace and methods to managed a matrix in cache
## Input  : x = Matrix to be solved
## Output : list of methods/functions to apply over the matrix passed. 
##          $get() return the matrix passed.
##          $getinverse() return the inverse matrix in cache
##          $setinverse() set the inverse in chache

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


## cacheSolve
## Check if the inverse of x has been computed and cached.
## If so, it returns the cached matrix and outputs a message accordingly
## If not, compute the inverse using solve() function and caches it.
## Input  : x = List returned by makeCacheMatrix
## Output : Return the inverse of cached matrix 
##          + Output also a message accordingly if the matrix was in cache
##          + If it is the first time, also store in cache the inverse obtained
 
cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      m <- x$getinverse()                       # Get inverse from par. list
      if(!is.null(m)) {                         # If inverse exist, 
            message("getting cached data")      # Output a message accordingly
            return(m)                           # Return inverse
      }
      data <- x$get()                           # Otherwise, get original matrix
      m <- solve(data, ...)                     # Compute inverse
      x$setinverse(m)                           # Store in cache
      m                                         # Return inverse
      
}

## 
## Example of use:
##
## # Let's generate a 3x3 matrix
## kk <- matrix(runif(n = 9, min = 0, max = 1), nrow = 3, ncol = 3) 
## kk_cache <- makeCacheMatrix(kk)              
## kk_result <- cacheSolve(kk_cache)            
## identical(kk_result, solve(kk))              # Check if inverse is ok.
## kk_result <- cacheSolve(kk_cache)            # will output a cache use messg