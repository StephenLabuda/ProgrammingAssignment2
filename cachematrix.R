## These functions will take the an input of a function and will then store
## that matrix 

## This function will create an object for which to pass a matrix to
## cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
  matr <- NULL
  set <- function(y){
    x <<- y
    matr <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) matr <<- inverse
  getinverse <- function() matr
  list(set = set, get = get, 
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function will solve for the inverse of a given matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  matr <- x$getinverse()
  if(!is.null(matr)){
    print("retrieving matrix")
    return(matr)
  }
  data <- x$get()
  inv_matr <- solve(data, ...)
  x$setinverse(inv_matr)
  return(inv_matr)
}
