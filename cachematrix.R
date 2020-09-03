## function to create a matrix object to cache its inverse

## first funtion is to create a matrix which contains list of several funtions
## for setting and getting the value of the matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function()x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function()m
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## function to compute the inverse of matrix returned by makeCacheMatrix
## or if it has been calculated then cacheSolve should retrieve inverse from cache

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)){
          message("getting cached data")
          return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
        
  }
