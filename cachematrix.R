## Create a special "matrix", a list with a function to:
##   - set and get the value of the matrix
##   - set and get the value of the inverse matrix


makeCacheMatrix <- function(x = matrix()) {
        invs <- NULL
        set <- function(y) {
                x <<- y
                invs <<- NULL
    
  }
  get <- function()x
  setinverse <- function(inv) invs <<- inv
  getinverse <- function() invs
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## This function calculates the inverse of the special "matrix" from makeCacheMatrix above. 
## If the inverse is calculated (and the matrix has not changed), 
## then the cachesolve should get the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invs <- x$getinverse()
        if(!is.null(invs)){
                message("getting cached data")
                return(invs)
        }
        data <- x$get()
        invs <- solve(data, ...)
        x$setinverse(invs)
        invs
}
