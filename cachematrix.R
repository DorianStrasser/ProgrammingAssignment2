## cacheSolve and makeCacheMatrix are ment to speed up matrix calculation.
## If the inverse of a matrix is allready calculated, it returns the cached value insted recalculating it.

## makeCacheMatrix creates a special matrix, which can store (set) and return (get) a matrix, but alos store (setinverse)
## return (getinverse) its inverse matrix.
##
## makeCacheMatrix expects a matrix as argument.

makeCacheMatrix <- function(x = matrix()) {
  ## Initialize inverse as empty
  i <- NULL
  
  ## set function takes a new matrix and delets the inverse
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  ## get function returns the matrix itself
  get <- function() x
  
  ## setinverse sets the value of the inverse matrix
  setinverse <- function(inverse) i <<- inverse
  
  ## getinverse returns the value of the inverse matrix
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}




## cacheSolve alows to calculate the inverse of matrix faster by utilizingt a CacheMatrix (see makeCacheMatrix).
## If the inverse of a matrix is allready calculated, it returns the cached value insted recalculating it.
##
## cacheSolve expects a matrix created by makeCacheMatrix as argument.

cacheSolve <- function(x) {
  ## Gets the saved inverse matrix
  i <- x$getinverse()
  
  ## If the inverse is not NULL (which means: allready calculated) the value is returned
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  ## Else: Get the matrix and calculate the inverse
  data <- x$get()
  i <- solve(data)
  
  ## Save the calculated value to cache an return the value
  x$setinverse(i)
  i  
}
