##The makeCacheMatrix function creates a Lexical Scope of parent,
##children functions to return the special matrix, then provide
##the inverse of the special matrix.  

##The makeCacheMatrix function is creating an open matrix "x"
##where the number of rows and columns will be assigned at a later
##time.

makeCacheMatrix <- function(x = numeric()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(x) inv <<- solve(x)
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

##The cacheSolve function is designed to return the inverse of
##the matrix "x" assigned in makeCacheMatrix.

cacheSolve <- function(x, ...) {
  inv_result<- x$getinv()
  if (!is.null(inv_result)){
    message("getting cached data")
    return(inv_result)
  }
  data <- x$get()
  inv_result <- solve(data)
  x$setinv(inv_result)
  inv_result
}
