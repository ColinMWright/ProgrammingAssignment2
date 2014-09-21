## cachematrix.R has two functions.
## The first makes a copy of an input matrix and allows its inverse to be stored.
## The second returns the inverse of its input matrix

## makeCacheMatrix() accepts a matrix and returns a list of 4 child functions: 
## 1. set the matrix (note: free variable y is scoped from parent environment)
## 2. get the matrix
## 3. set the inverse matrix (note:free variable m is scoped from parent as NULL)
## 4. get the inverse matrix

makeCacheMatrix <- function(x = matrix()){
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set=set, get=get, 
       setinverse=setinverse,
       getinverse=getinverse)
}


## cacheSolve returns a matrix that is the inverse of 'x'.
## If the inverse has already been calculated, the cached matrix is returned.
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
