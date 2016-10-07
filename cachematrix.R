## The below two functions create an object that stores
## and matrix and captures its inverse, meaning that 
##the inverse of the matrix doesn't have to be computed 
## repeatedly

## Creates a matrix object that caches the inverse

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
set <- function(y) {
  x <<-y
  inv <<- NULL
}
get<-function() x
setinverse <- function(inverse) inv <<-inverse
getinverse <-function() inv
list(set=set,
     get=get,
     setinverse=setinverse,
     getinverse=getinverse)
}


## Computes the inverse of the matrix created by the 
## above function. If the inverse has been calculated 
## already, then it retrieves the inverse from the cache

CacheSolve <- function(x, ...) {
inv <- x$getinverse()
if (!is.null(inv)) {
  message("data cache")
  return(inv)
}
mat <- x$get()
inv <-solve(mat,...)
x$setinverse(inv)
inv
}
