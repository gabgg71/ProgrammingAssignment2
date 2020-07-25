## There are two function the idea is find the inverse of a matrix but saving the cache to optimize algorithm, the right way to
## use this program is: 1)Create a matrix (preferably assigned to a variable) 2) variable_ <- makeCacheMatrix(putMatrixHere) 
##3) you can use variable_$someFunction to obtain information about that matrix or change it
##4) cacheSolve(variable_) show you the inverse of the matrix or say you if that inverse is in cache
##5) variable_$getInverse to check the changes or variable_$set to do the proccess with other matrix.
## NOTE: variable_ is just a name to do the example it could be whatever you want and someFuntion could be: get, set, getInverse or setInverse



## This function receives a matrix and it have multiple function that permit get or set the matrix
## set or get the inverse 
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL 
  set <- function(y) {
    x <<- y 
    i <<- NULL 
  }
  get <- function() x 
  setInverse <- function(inverse) i <<- inverse 
  getInverse <- function() i 
  list(set = set, get = get, 
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve return the inverse of the matrix, it we had used before with a matrix the inverse is in cache if
## not the function is gonna calculate it with solve function.

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if(!is.null(i)) {  
    message("getting cached data")
    i
  }
  data <- x$get() 
  tryCatch(i <- solve(data, ...), error = function(x, ...) print("This is NOT an invertible matrix, try with other"), finally=NULL)
  x$setInverse(i)
  i  ## Return a matrix that is the inverse of 'x'
}
