## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

library(matlib) # import libraries function to calculate inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL       # initializing inverse as NULL
    set <- function(y) {
                        x <<- y
                        inv <<- NULL
                        }
    get <- function() x # function for matrix
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() {
                          invt <- Ginv(x)
                          invt%*%x    #function to get inverse
    }
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function
## This is used to get cache data

cacheSolve <- function(x, ...) {    # to gets caches data 
    inv <- x$getinv()
    if(!is.null(inv)) {         # to check if inverse is NULL
      message("getting cached data")
      return(inv)
    }
    data <- x$get()
    inv <- inv(data, ...) # for calculating the inverse matrix
    x$setinv(inv)
  
     inv   ## Return a matrix that is the inverse of 'x'
}
