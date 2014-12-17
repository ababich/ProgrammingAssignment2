## The pair of functions below creates a cacheable matrix object
## and allows to store inverse matrix calculation result in memory to reuse
## solve() is used for calculation

## Returns cacheable matrix object
makeCacheMatrix <- function(x = matrix()) {
  inv_m <- NULL
  set <- function(y) {
    x <<- y
    inv_m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv_m <<- inverse
  getinverse <- function() inv_m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Returns inverse of matrix object created using `makeCacheMatrix`
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv_m <- x$getinverse()
  if(!is.null(inv_m)) {
    # message("getting cached data")
    return(inv_m)
  }
  data <- x$get()
  inv_m <- solve(data, ...) # solve() is the function to get inverse of a square matrix
  x$setinverse(inv_m)
  inv_m
}
