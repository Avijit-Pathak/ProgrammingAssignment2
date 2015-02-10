## makeCacheMatrix creates a special "Matrix"
## CacheSolve function calculates the mean of the special "Matrix" created with the above function
## however it checks the cache to see if the mean is already calculated

## makeCacheMatrix creates special "Matix"

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  get <- function()x
  setinverse <- function(inverse)inv<<-inverse
  getinverse <- function()inv
  list(set=set, get=get,setinverse=setinverse,getinverse=getinverse)
}


## cacheSolve calculates the inverse of matrix but if it is already calculted
## it gets it from the cache

cacheSolve <- function(x, ...) {
  #returning a matrix inverse of x
  inv <- x$getinverse()
  #checking for cached matrix
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setinverse(inv)
  inv
}
