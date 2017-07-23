## Below functions helps cache the inverse of a matrix, and retrieve the result if a same matrix is to be inversed previously
## They could be used to avoid computing matrix inversion repeatedly, which is costly

## makeCacheMatrix creates a special "matrix", which is really a list containing a functions to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse of the matrix
# 4. get the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set,
       get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}


## cacheSolve calculates the inverse of the special "matrix" created with makeCacheMatrix
# However, it first checks to see if the inverse has already been calculated
# -> if so, it gets the mean from the cache and skips the computation
# -> Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinverse function.
# It assumes that the matrix supplied is always invertible.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

## Testing:
# > test <- matrix(rnorm(4,1),2)
# > test_cache <- makeCacheMatrix(test)
# > test_cache$get()
# [,1]       [,2]
# [1,]  1.517630 1.82365567
# [2,] -0.550157 0.08257022
# > cacheSolve(test_cache)
# [,1]      [,2]
# [1,] 0.07316112 -1.615845
# [2,] 0.48746512  1.344691
## running cacheSolve(test_cache) again, and this time message "getting cached data." should appear
# > cacheSolve(test_cache)
# getting cached data.
# [,1]      [,2]
# [1,] 0.07316112 -1.615845
# [2,] 0.48746512  1.344691
> 
