# What makeCacheMatrix function does?

# 1. It sets the value of a matrix
# 2. It gets the value of a matrix
# 3. It sets the value of the inverse of the matrix
# 4. It gets the value of the inverse of the matrix


makeCacheMatrix <- function(X = matrix()){
  m <- NULL
  set <- function(y){
    X <<- y
    m <<- NULL
  }
  get <- function() X
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set=set, get=get,
       setInverse =setInverse,
       getInverse = getInverse)
  
  
}
  
# What cacheSolve function does?
# It computes the inverse an invertible matrix returned by makeCacheMatrix above.

# 1. It checks if the inverse of the matrix has already been calculated.
#   A. If the matrix has already been calculated: it gets the inverse from the cache and skips the computation.
#   B. If the matrix has not been calculated yet: 
#       A.1. It calculates the inverse of the matrix.
#       A.2. It sets the value of the inverse in the cache via the setInverse function.

  
cacheSolve <- function(X, ...){
  
  m <- X$getInverse()
  if (!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- X$get()
  m <- solve(data, ...)
  X$setInverse(m)
  m
  
}
