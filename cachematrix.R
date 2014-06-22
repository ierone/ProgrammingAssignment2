## this function initialies the globals that shall be used to compute the inverse of the matrix
## In these functions I do not consider the fundamental rules that have to be passed for solve() to work.
## 1. - the tatrix has to be square
## 2. - the matrix has to be invertible
## This is left at the discresion of the user.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setSolve <- function(solve) m <<- solve
  getSolve <- function() m
  list(set = set, 
       get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}

#test the makeCacheMatrix functions
## I test using the simplest square matrix a 2X2 with values 1:4
v <- makeCacheMatrix()
v$set(matrix(data=1:4, 	nrow = 2, ncol = 2, byrow = F))
v$get()



## the cacheSolve function is supposed to check if the inverse of
## matrix is already computed else it computes it and returns its inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
m <- x$getSolve()
  if(!is.null(m)) {
    message("getting cached inverse of the matrix")
    return(m)
  }

  data <- x$get()
  m <- solve(data, ...)
  x$setSolve(m)
  m

}


## test cacheSolve function
cacheSolve(v)


##check if it reads from cache again
cacheSolve(v)


