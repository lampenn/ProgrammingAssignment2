## cachematrix.R contains a pair of functions caches the inverse of a matrix


## makecacheMatrix takes a matrix 'x' and returns 
## a list of functions that can store a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {

     invx <<- NULL
     
     set <- function(newx){
          x <<- newx
          invx <<- NULL
     }
     
     get <- function() x
     setinverse <- function(i) invx <<- i
     getinverse <- function() invx
     
     ## list returned by makeCacheMatrix
     list(set = set, get = get, 
          setinverse = setinverse, getinverse = getinverse)
 
}



## cacheSolve takes an instance of makeCacheMatrix 'x' and 
## returns the cached inverse matrix

cacheSolve <- function(x, ...) {
     
     m <- x$getinverse()
     
     if(!is.null(m)) {
          message("matrix already exists")
          return(m)
     }

     mtemp <- x$get()
     m <- solve(mtemp)
     x$setinverse(m)
     m
     
}
