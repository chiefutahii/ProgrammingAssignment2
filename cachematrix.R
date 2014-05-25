## Matrix inversion is usually a costly computation, so these functions
## calculate the inversion and cache the result.  The function then will
## check to see if the inversion has already been calculated, and if it
## has, it will just pull the result from the cache rather than calculating
## it again.

## The makeCacheMatrix creates an invertible matrix and caches it in memory

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setMatrix <- function(matrix) m <<- matrix
      getInverse <- function() m
      list(set = set, get = get,
           setMatrix = setMatrix,
           getInverse = getInverse) ## creates a list with the original matrix
                                    ## and the inverse
}


## The cacheSolve matrix checks to see if the inverse of a matrix has
## already been solved.  If it has, it pulls the cached data.  If it
## hasn't, it uses the solve() function in R to calculate the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      m <- x$getInverse() ## Create a variable to check if the inverse is cached
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      } ## This if statement checks to see if the cache exists.  If it does, 
        ## it returns the cached result
      data <- x$get()  ## brings in the original matrix
      m <- solve(data, ...)  ## calculates the inverse matrix
      x$setMatrix(m) ## sets the result of the inverse calculation to the cache
      m ## returns the matrix inverse
}
